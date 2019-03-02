{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings, DeriveAnyClass #-}

module GUI (runGUI) where

import Core
import MidiCore
import Output
import OutputCore

import Control.Concurrent (forkIO)
import Control.Exception (Exception, throw)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.IORef (readIORef, newIORef, writeIORef)
import Data.Maybe (maybe)

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core hiding (empty)

data GUIException = UnexpectedSelectionException
  deriving (Show, Exception)

makeUIOption :: (String, String) -> UI Element
makeUIOption (t, v) = UI.option # set UI.text t # set UI.value v

setButtonLatch :: Bool -> UI Element -> UI Element
setButtonLatch b = set UI.style [("border-style", if b then "inset" else "outset")]

latchingButton :: UI Element
latchingButton = UI.button # setButtonLatch False
                           # set UI.class_ "form-control"

hideElement :: UI Element -> UI Element
hideElement = set UI.style [("display", "none")]

showElement :: UI Element -> UI Element
showElement = set UI.style [("display", "inline")]

data Selecting = SelectingControl | SelectingChannel | SelectingAction | SelectingNone
data Selected = SelectedControl Control | SelectedChannel Channel | SelectedAction Action | SelectedNone
instance Show Selected where
  show (SelectedControl c)  = "Control: " ++ show c
  show (SelectedChannel ch) = "Channel: " ++ show ch
  show (SelectedAction  a)  = "Action: "  ++ show a
  show  SelectedNone        = "Nothing selected"

-- BOOTSTRAP
makeCol :: Bool -> [UI Element] -> UI Element
makeCol tight = (UI.div # set UI.class_ (if tight then "col-sm-auto px-0" else "col-sm-auto") #+)

makeRow :: [UI Element] -> UI Element
makeRow = (UI.div # set UI.class_ "row" #+)

makeContainer :: [[UI Element]] -> UI Element
makeContainer = (UI.div # set UI.class_ "container" #+) . map makeRow

buildPage :: State -> UI.Window -> UI ()
buildPage State{..} win = do
--  UI.addStyleSheet win "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
  _ <- return win # set UI.title "midi2osc"
  bankLabel <- UI.label

  controlButton <- latchingButton # set UI.text "Select control"
  channelButton <- latchingButton # set UI.text "Select channel"
  actionButton  <- latchingButton # set UI.text "Select action"

  selectedLabel <- UI.label # set UI.class_ "form-text" # set UI.text "Nothing selected"

  outputType          <- UI.select # set UI.class_ "form-control" # hideElement
  outputTypeArg       <- UI.select # set UI.class_ "form-control" # hideElement
  outputActionPreset  <- UI.select # set UI.class_ "form-control" # hideElement
  outputActionField   <- UI.input  # set UI.class_ "form-control" # hideElement
  outputChannelPreset <- UI.select # set UI.class_ "form-control" # hideElement
  outputChannelField  <- UI.input  # set UI.class_ "form-control" # hideElement

  currentControlLabel <- UI.label # set UI.text "No control moved"
  _ <- getBody win #+
    [ makeContainer
      [ [makeCol False [UI.h1 # set UI.text "MIDI2OSC control panel"]]
      , [makeCol False [UI.h3 # set UI.children [bankLabel]]]
      , [ makeCol False
            [ makeRow [makeCol False [element controlButton]]
            , makeRow [makeCol False [element channelButton]]
            , makeRow [makeCol False [element actionButton]]
            ]
        , makeCol False [element selectedLabel]
        , makeCol True  [element outputType]
        , makeCol True  [element outputTypeArg]
        , makeCol True  [element outputActionPreset]
        , makeCol True  [element outputActionField]
        , makeCol True  [element outputChannelPreset]
        , makeCol True  [element outputChannelField]
        ]
      , [makeCol False [UI.hr]]
      , [makeCol False [element currentControlLabel]]
      ]
    ]

  selecting <- liftIO . newIORef $ SelectingNone
  selected  <- liftIO . newIORef $ SelectedNone

  _ <- liftIO . register eBankSwitch $ \_ -> runUI win $ do
    i <- liftIO . readIORef $ currentMappingIndex
    _ <- element bankLabel # set UI.text ("Bank: " ++ show i)
    return ()

  liftIO (hBankSwitch ())

  let hideAll :: UI ()
      hideAll = do
        _ <- element outputType          # hideElement
        _ <- element outputTypeArg       # hideElement
        _ <- element outputActionPreset  # hideElement
        _ <- element outputActionField   # hideElement
        _ <- element outputChannelPreset # hideElement
        _ <- element outputChannelField  # hideElement
        return ()

  let setSelecting :: Selecting -> UI ()
      setSelecting SelectingControl = do
        _ <- element controlButton # setButtonLatch True
        _ <- element channelButton # setButtonLatch False
        _ <- element  actionButton # setButtonLatch False
        liftIO (writeIORef selecting SelectingControl)
        hideAll
      setSelecting SelectingChannel = do
        _ <- element controlButton # setButtonLatch False
        _ <- element channelButton # setButtonLatch True
        _ <- element  actionButton # setButtonLatch False
        liftIO (writeIORef selecting SelectingChannel)
        hideAll
      setSelecting SelectingAction = do
        _ <- element controlButton # setButtonLatch False
        _ <- element channelButton # setButtonLatch False
        _ <- element  actionButton # setButtonLatch True
        liftIO (writeIORef selecting SelectingAction)
        hideAll
      setSelecting SelectingNone = do
        _ <- element controlButton # setButtonLatch False
        _ <- element channelButton # setButtonLatch False
        _ <- element  actionButton # setButtonLatch False
        liftIO (writeIORef selecting SelectingNone)
        hideAll

  on UI.click controlButton . const $ setSelecting SelectingControl
  on UI.click channelButton . const $ setSelecting SelectingChannel
  on UI.click  actionButton . const $ setSelecting SelectingAction

  let respondToMoved :: ControlState -> IO ()
      respondToMoved controlState = runUI win $ do
        let ControlState control _ = controlState
        selectingB <- liftIO . readIORef $ selecting
        runMaybeT_ $ do
          case selectingB of
            SelectingControl -> lift . liftIO . writeIORef selected . SelectedControl $ control
            SelectingChannel -> do
              ch <- MaybeT . return . lookup control $ channelGroups
              liftIO . writeIORef selected . SelectedChannel $ ch
            SelectingAction -> do
              a <- MaybeT . return . lookup control $ actionGroups
              liftIO . writeIORef selected . SelectedAction $ a
            SelectingNone   -> fail ""
          lift $ setSelecting SelectingNone
          _ <- lift $ (element selectedLabel #) . set UI.text . show =<< liftIO (readIORef selected)
          _ <- lift $ element outputType # showElement
                                         # set UI.children []
                                         #+ map makeUIOption (("Clear", "clear") : outputTypes)
                                         # set UI.selection Nothing
          return ()

        mapping <- liftIO currentMapping
        _ <- element currentControlLabel # set UI.text (show controlState ++ maybe ":\tNot mapped" (":\tMapping: " ++) (show <$> outputForControl State{..} mapping control))
        return ()

  let respondToOutputTypeChanged :: a -> IO ()
      respondToOutputTypeChanged _ = runUI win . runMaybeT_ $ do
        _ <- MaybeT $ outputType # get UI.selection
        lift $ do
          oType <- outputType # get UI.value
          case oType of
            "" -> hideAll
            "clear" -> do
              (liftIO . readIORef $ selected) >>= \case
                SelectedControl c -> do
                  liftIO $ removeControlOutput State{..} c
                  return ()
                SelectedChannel ch -> do
                  liftIO $ removeChannelOutput State{..} ch
                  return ()
                SelectedAction a -> do
                  liftIO $ removeActionOutput State{..} a
                  return ()
                SelectedNone -> return ()
              hideAll
            _ -> do
              _ <- element outputTypeArg # showElement
                                         # set UI.children []
                                         #+ map makeUIOption (outputTypeArgs State{..} ! oType)
                                         # set UI.selection Nothing
              return ()

  let respondToOutputTypeArgChanged :: a -> IO ()
      respondToOutputTypeArgChanged _ = runUI win . runMaybeT_ $ do
        _ <- MaybeT $ outputType # get UI.selection
        lift $ do
          oType <- outputType # get UI.value
          (liftIO . readIORef $ selected) >>= \case
            SelectedControl _ -> do
              _ <- element outputActionPreset # showElement
                                              # set UI.children []
                                              #+ map makeUIOption ((outputPresetsOfType ! oType) ++ (outputActionPresetsOfType ! oType))
                                              # set UI.selection Nothing
              return ()
            SelectedChannel _ -> do
              _ <- element outputChannelPreset # showElement
                                               # set UI.children []
                                               #+ map makeUIOption (outputChannelPresetsOfType ! oType)
                                               # set UI.selection Nothing
              return ()
            SelectedAction _ -> do
              _ <- element outputActionPreset # showElement
                                              # set UI.children []
                                              #+ map makeUIOption (outputActionPresetsOfType ! oType)
                                              # set UI.selection Nothing
              return ()
            SelectedNone -> return ()

  let setControl :: Output -> UI ()
      setControl o = (liftIO . readIORef $ selected) >>= \case
        SelectedControl c -> do
          liftIO $ updateControlOutput State{..} c o
          hideAll
        _ -> throw UnexpectedSelectionException

  let setAction :: OutputAction -> UI ()
      setAction o = (liftIO . readIORef $ selected) >>= \case
        SelectedAction a -> do
          liftIO $ updateActionOutput State{..} a o
          hideAll
        SelectedControl _ -> do
          oType <- outputType # get UI.value
          _ <- element outputChannelPreset # showElement
                                           # set UI.children []
                                           #+ map makeUIOption (outputChannelPresetsOfType ! oType)
                                           # set UI.selection Nothing
          return ()
        _ -> throw UnexpectedSelectionException

  let getOutputActionPreset :: MaybeT UI String
      getOutputActionPreset = do
        _ <- MaybeT $ outputActionPreset # get UI.selection
        lift $ outputActionPreset # get UI.value

  let respondToOutputActionPresetChanged :: a -> IO ()
      respondToOutputActionPresetChanged _ = runUI win . runMaybeT_ $ do
        oActionPreset <- getOutputActionPreset
        oTypeArg <- lift $ outputTypeArg # get UI.value
        (
          (liftIO . readIORef $ selected) >>= \case
            SelectedControl _ ->
              flip runFactory oTypeArg <$> (MaybeT . return $ lookup oActionPreset outputPresets) >>= \case
                Value v    -> lift . setControl $ v
                Function _ -> do
                  _ <- lift $ element outputActionField # showElement
                                                         # set UI.value ""
                  return ()
            _ -> fail ""
          ) <|> lift (
          case outputActionPresets ! oActionPreset of
            Value v    -> setAction v
            Function _ -> do
              _ <- element outputActionField # showElement
                                             # set UI.value ""
              return ()
          )
        return ()

  let respondToOutputActionFieldChanged :: a -> IO ()
      respondToOutputActionFieldChanged _ = runUI win . runMaybeT_ $ do
        oActionPreset <- getOutputActionPreset
        oTypeArg <- lift $ outputTypeArg # get UI.value
        (
          (liftIO . readIORef $ selected) >>= \case
            SelectedControl _ ->
              flip runFactory oTypeArg <$> (MaybeT . return $ lookup oActionPreset outputPresets) >>= \case
                Value v    -> lift . setControl $ v
                Function f -> lift $ setControl . f =<< outputActionField # get UI.value
            _ -> fail ""
          ) <|> lift (
          case outputActionPresets ! oActionPreset of
            Value v    -> setAction v
            Function f -> setAction . f =<< outputActionField # get UI.value
          )
        return ()

  let setChannel :: OutputChannel -> UI ()
      setChannel o = do
        (liftIO . readIORef $ selected) >>= \case
          SelectedChannel a -> liftIO $ updateChannelOutput State{..} a o
          SelectedControl c -> runMaybeT_ $ do
            oActionPreset <- lift $ outputActionPreset # get UI.value
            oA <- case outputActionPresets ! oActionPreset of
              Value v    -> return v
              Function f -> lift $ f <$> outputActionField # get UI.value
            o' <- MaybeT . return $ outputCombine o oA
            lift . liftIO $ updateControlOutput State{..} c o'
          _ -> throw UnexpectedSelectionException
        hideAll

  let respondToOutputChannelPresetChanged :: a -> IO ()
      respondToOutputChannelPresetChanged _ = runUI win . runMaybeT_ $ do
        _ <- MaybeT $ outputChannelPreset # get UI.selection
        lift $ do
          oChannelPreset <- outputChannelPreset # get UI.value
          oTypeArg <- outputTypeArg # get UI.value
          case runFactory (outputChannelPresets ! oChannelPreset) oTypeArg of
            Value v    -> setChannel v
            Function _ -> do
              _ <- element outputChannelField # showElement
                                              # set UI.value ""
              return ()

  let respondToOutputChannelFieldChanged :: a -> IO ()
      respondToOutputChannelFieldChanged _ = runUI win . runMaybeT_ $ do
        _ <- MaybeT $ outputChannelPreset # get UI.selection
        _ <- MaybeT $ outputTypeArg # get UI.selection
        lift $ do
          oChannelPreset <- outputChannelPreset # get UI.value
          oChannelArg    <- outputChannelField  # get UI.value
          oTypeArg <- outputTypeArg # get UI.value
          case runFactory (outputChannelPresets ! oChannelPreset) oTypeArg of
            Value v    -> setChannel v
            Function f -> setChannel . f $ oChannelArg

  _ <- liftIO . register eMoved $ respondToMoved
  _ <- liftIO . register (UI.selectionChange outputType) $ respondToOutputTypeChanged
  _ <- liftIO . register (UI.selectionChange outputTypeArg) $ respondToOutputTypeArgChanged
  _ <- liftIO . register (UI.selectionChange outputActionPreset) $ respondToOutputActionPresetChanged
  _ <- liftIO . register (UI.blur outputActionField) $ respondToOutputActionFieldChanged
  _ <- liftIO . register (UI.keypress outputActionField) $ (\case { '\r' -> respondToOutputActionFieldChanged (); _ -> return () })
  _ <- liftIO . register (UI.selectionChange outputChannelPreset) $ respondToOutputChannelPresetChanged
  _ <- liftIO . register (UI.blur outputChannelField) $ respondToOutputChannelFieldChanged
  _ <- liftIO . register (UI.keypress outputChannelField) $ (\case { '\r' -> respondToOutputChannelFieldChanged (); _ -> return () })

  return ()

runGUI :: State -> IO ()
runGUI = void . forkIO . startGUI defaultConfig
    { jsPort   = Just 8023
    , jsAddr   = Just "0.0.0.0"
    , jsCustomHTML = Just "custom.html"
    , jsStatic = Just "static"
    } . buildPage
