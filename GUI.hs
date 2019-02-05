module GUI (runGUI) where

import Core
import Midi
import Output

import Prelude hiding (lookup)
import Control.Concurrent
import Control.Monad
import qualified Data.Array as Array
import Data.Array hiding ((!))
import Data.IORef
import Data.Map.Strict (lookup, empty, (!), insert, fromList, toList)
import Data.Maybe

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core hiding (empty)
import           Reactive.Threepenny         hiding (empty)

makeUIOption :: (String, String) -> UI Element
makeUIOption (t, v) = UI.option # set UI.text t # set UI.value v

buildPage :: State -> UI.Window -> UI ()
buildPage state win = do
  return win # set UI.title "midi2osc"
  bankLabel <- UI.label
  button <- UI.button # set UI.text "Select control"
                      # set UI.style [("border-style","outset")]
  controlLabel <- UI.label # set UI.text "Please select a control first"
  outputType <- UI.select # set UI.enabled False
                          # set UI.children []
                          #+ map makeUIOption outputTypes
  outputPreset <- UI.select # set UI.enabled False
  outputField <- UI.input # set UI.enabled False
  currentControlLabel <- UI.label # set UI.text "No control moved"
  getBody win #+
    [ UI.h1 # set UI.text "MIDI2OSC control panel"
    , element bankLabel
    , element button
    , element controlLabel
    , element outputType
    , element outputPreset
    , element outputField
    , UI.hr
    , element currentControlLabel
    ]

  unregBankSwitch <- liftIO $ register (eBankSwitch state) $ \_ -> runUI win $ do
    i <- liftIO . readIORef . currentMappingIndex $ state
    element bankLabel # set UI.text ("Bank: " ++ show i)
    return ()

  liftIO (hBankSwitch state ())

  on UI.click button $ const $ do
    element button # set UI.style [("border-style","inset")]
    liftIO (writeIORef (selecting state) True)

  unregEnter <- liftIO $ newIORef (return ())

  unregMoved <- liftIO $ register (eMoved state) $ \controlState -> runUI win $ do
    let control = midiControlFromState controlState
    selectingB <- liftIO . readIORef . selecting $ state
    if selectingB then do
      liftIO (writeIORef (selecting state) False)
      element button # set UI.style [("border-style","outset")]
      element controlLabel # set UI.text (show control)
      element outputType # set UI.enabled True

      liftIO . join $ readIORef unregEnter
      liftIO . writeIORef unregEnter =<< (liftIO $ register (UI.keypress outputField) $ \key -> runUI win $ case key of
        '\r' -> do
          oTypeI <- get UI.selection outputType
          let oType = fromMaybe "" (snd . (outputTypes !!) <$> oTypeI)
          presetI <- get UI.selection outputPreset
          let preset = fromMaybe "" (snd . ((outputPresetsOfType ! oType) !!) <$> presetI)
          let presetF = outputPresets ! preset
          newOutput <- read . (oType ++) . (' ' :) . presetF <$> (element outputField >>= get UI.value)

          element outputType # set UI.enabled False
                             # set UI.selection (Just 0)
          element outputPreset # set UI.enabled False
                               # set UI.selection (Just 0)
          element outputField # set UI.enabled False
                              # set UI.value ""
          i <- liftIO . readIORef . currentMappingIndex $ state
          liftIO (modifyIORef' (mappings state) (\ms -> ms // [(i, insert control newOutput (ms Array.! i))]))
          liftIO $ currentMapping state >>= save (filenames state !! i)
          case control of
            MidiButton n -> liftIO (modifyIORef' (buttonStates state) (insert n False))
            _ -> return ()
        _ -> return ()
        )
      return ()
    else return ()
    mapping <- liftIO $ currentMapping state
    element currentControlLabel  # set UI.text (show controlState ++ maybe "\tNot mapped" ("\tMapping: " ++) (show <$> lookup control mapping))
    return ()

  unregSelectOutputType <- liftIO $ register (UI.selectionChange outputType) $ \oTypeI -> runUI win $ do
    let oType = fromMaybe "" (snd . (outputTypes !!) <$> oTypeI)
    if oType == ""
      then do
        element outputPreset # set UI.enabled False
                             # set UI.children []
        element outputField # set UI.enabled False
                            # set UI.value ""
      else element outputPreset # set UI.enabled True
                                # set UI.children []
                                #+ map makeUIOption (outputPresetsOfType ! oType)
    return ()

  unregSelectOutputPreset <- liftIO $ register (UI.selectionChange outputPreset) $ \presetI -> runUI win $ do
    oTypeI <- get UI.selection outputType
    let oType = fromMaybe "" (snd . (outputTypes !!) <$> oTypeI)
    let preset = fromMaybe "" (snd . ((outputPresetsOfType ! oType) !!) <$> presetI)
    if preset == ""
      then element outputField # set UI.enabled False
                               # set UI.value ""
      else element outputField # set UI.enabled True
    return ()

  return ()


runGUI :: State -> IO ()
runGUI = (>> return ()) . forkIO . startGUI defaultConfig
    { jsPort   = Just 8023
    } . buildPage

