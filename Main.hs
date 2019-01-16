{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

import Prelude hiding (readFile, writeFile)
import Sound.PortMidi
import Control.Monad
import Control.Concurrent
import Control.Exception (throw, throwIO, catch, IOException)
import System.IO.Error
import Data.List
import Data.Word
import Data.Tuple
import Data.Maybe
import Data.IORef
import Data.Bool
import Data.ByteString (readFile, writeFile)
import Data.Array
import qualified Data.Serialize as Serialize
import qualified Data.Map.Strict as Map
import Text.Printf
import System.Environment
import GHC.Generics

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Reactive.Threepenny

import qualified Sound.OSC              as OSC
import qualified Sound.OSC.Transport.FD as OSC.FD

upTo :: Int -> [Int]
upTo n = [0..n-1]

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x, y) = (x, f y)

mkArray :: [a] -> Array Int a
mkArray xs = listArray (0, length xs - 1) xs

bytes :: Integral a => a -> [Word8]
bytes = unfoldr f
  where f 0 = Nothing
        f n = Just . mapFst fromIntegral . swap . divMod n $ 256

prettyDevice :: DeviceID -> IO String
prettyDevice id = f <$> getDeviceInfo id
  where f (DeviceInfo _ nm True  False _) = show id ++ ": " ++ nm ++ " - Input"
        f (DeviceInfo _ nm False True  _) = show id ++ ": " ++ nm ++ " - Output"
        f (DeviceInfo _ nm True  True  _) = show id ++ ": " ++ nm ++ " - Input/Output"

data MidiControl = MidiButton Word8 | MidiFader Word8 | MidiUnknown
  deriving (Show, Eq, Ord, Generic)

instance Serialize.Serialize MidiControl

data MidiControlState = MidiButtonState Word8 Bool | MidiFaderState Word8 Word8 | MidiUnknownState
  deriving (Show, Eq, Ord)

midiControlFromState :: MidiControlState -> MidiControl
midiControlFromState (MidiButtonState n _) = MidiButton n
midiControlFromState (MidiFaderState  n _) = MidiFader n
midiControlFromState  MidiUnknownState     = MidiUnknown

midiValueFromState :: State -> MidiControlState -> IO Float
midiValueFromState state (MidiButtonState n True ) = modifyIORef' (buttonStates state) (Map.update (Just . not) n)
                                                  >> bool 0.0 1.0 . fromMaybe False . Map.lookup n <$> readIORef (buttonStates state)
midiValueFromState state (MidiButtonState n False) = bool 0.0 1.0 . fromMaybe False . Map.lookup n <$> readIORef (buttonStates state)
midiValueFromState _     (MidiFaderState  _ v    ) = return ((fromIntegral v) / 127.0)
midiValueFromState _      MidiUnknownState         = return 0.0

processEvents :: PMStream -> State -> IO ()
processEvents stream state = do
  events <- either (error "Cannot get events") id <$> readEvents stream
  let controls = map (getControl . bytes . message) $ events
  selectingB <- readIORef . selecting $ state
  if not selectingB then sequence . map (respondToControl state) $ controls else return []
  if not . null $ controls then hMoved state . last $ controls else return ()
  --if not . null $ controls then print controls else return ()
  processEvents stream state

printMessage :: [Word8] -> IO ()
printMessage = putStrLn . unwords . map (printf "0x%02x")

getControl :: [Word8] -> MidiControlState
getControl ([0x90,n,0x7F]) = MidiButtonState n True
getControl ([0x80,n,0x7F]) = MidiButtonState n False
getControl ([0xB0,n     ]) = MidiFaderState n 0
getControl ([0xB0,n,l   ]) = MidiFaderState n l
getControl   _             = MidiUnknownState

data Output = Print String | OSC String | BankSwitch Int
  deriving (Show, Read, Generic)

instance Serialize.Serialize Output

outputTypes :: [(String, String)]
outputTypes = [("--", ""), ("OSC", "OSC"), ("Switch Bank", "BankSwitch")]

outputPresetsOfType :: Map.Map String [(String, String)]
outputPresetsOfType = Map.fromList
  [ ("OSC",
    [ ("--", "")
    , ("Fade ch", "OSCfade")
    , ("Mute ch", "OSCmute")
    , ("Gain ch", "OSCgain")
    , ("Other", "OSCother")
    ])
  , ("BankSwitch",
    [ ("--", "")
    , ("number", "BankSwitch")
    ])
  ]

outputPresets :: Map.Map String (String -> String)
outputPresets = Map.fromList
  [ ("OSCfade", printf "\"/ch/%02d/mix/fader\"" . (read :: String -> Int))
  , ("OSCmute", printf "\"/ch/%02d/mix/on\"" . (read :: String -> Int))
  , ("OSCgain", printf "\"/headamp/%02d/gain\"" . (read :: String -> Int))
  , ("OSCother", ("\"" ++) . (++ "\""))
  , ("BankSwitch", id)
  ]

type Mapping = Map.Map MidiControl Output
type ButtonStates = Map.Map Word8 Bool

respondToControl :: State -> MidiControlState -> IO ()
respondToControl state control = Map.lookup (midiControlFromState control) <$> currentMapping state >>= (\o -> case o of
  Just (Print x) -> print x
  Just (OSC path) -> sendOSC . OSC.Message path . (:[]) . OSC.Float =<< midiValueFromState state control
  Just (BankSwitch n) -> writeIORef (currentMappingIndex state) n >> hBankSwitch state ()
  Nothing -> return ()
  )

sendOSC :: OSC.Message -> IO ()
sendOSC msg = OSC.FD.withTransport oscUDP (\fd -> OSC.FD.sendMessage fd msg)

oscUDP :: IO OSC.UDP
oscUDP = OSC.openUDP "192.168.1.1" 10024

save :: String -> Mapping -> IO ()
save filename mapping = writeFile filename . Serialize.encode $ mapping

open :: String -> IO Mapping
open filename = catch (either (const . throwIO . userError $ "Invalid file") return . Serialize.decode =<< readFile filename) (const . return $ Map.empty :: IOException -> IO Mapping)

currentMapping :: State -> IO Mapping
currentMapping state = (!) <$> readIORef (mappings state) <*> readIORef (currentMappingIndex state)

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
    [ element bankLabel
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
      element controlLabel # set UI.text ("Control: " ++ case control of
          MidiButton n -> "Button " ++ show n
          MidiFader  n -> "Fader "  ++ show n
          MidiUnknown  -> "Unknown"
        )
      element outputType # set UI.enabled True

      liftIO . join $ readIORef unregEnter
      liftIO . writeIORef unregEnter =<< (liftIO $ register (UI.keypress outputField) $ \key -> runUI win $ case key of
        '\r' -> do
          oTypeI <- get UI.selection outputType
          let oType = fromMaybe "" (snd . (outputTypes !!) <$> oTypeI)
          presetI <- get UI.selection outputPreset
          let preset = fromMaybe "" (snd . ((outputPresetsOfType Map.! oType) !!) <$> presetI)
          let presetF = outputPresets Map.! preset
          newOutput <- read . (oType ++) . (' ' :) . presetF <$> (element outputField >>= get UI.value)

          element outputType # set UI.enabled False
                             # set UI.selection (Just 0)
          element outputPreset # set UI.enabled False
                               # set UI.selection (Just 0)
          element outputField # set UI.enabled False
                              # set UI.value ""

          i <- liftIO . readIORef . currentMappingIndex $ state
          liftIO (modifyIORef' (mappings state) (\ms -> ms // [(i, Map.insert control newOutput (ms ! i))]))
          liftIO $ currentMapping state >>= save (filenames state !! i)
          case control of
            MidiButton n -> liftIO (modifyIORef' (buttonStates state) (Map.insert n False))
            _ -> return ()
        _ -> return ()
        )
      return ()
    else return ()
    mapping <- liftIO $ currentMapping state
    element currentControlLabel  # set UI.text ("Control: " ++ case controlState of
        MidiButtonState n v -> "Button " ++ show n ++ "\tLevel: " ++ show v
        MidiFaderState  n v -> "Fader "  ++ show n ++ "\tLevel: " ++ show v
        MidiUnknownState    -> "Unknown"
      ++ maybe "\tNot mapped" ("\tMapping: " ++) (show <$> Map.lookup control mapping))
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
                                #+ map makeUIOption (outputPresetsOfType Map.! oType)
    return ()
  unregSelectOutputPreset <- liftIO $ register (UI.selectionChange outputPreset) $ \presetI -> runUI win $ do
    oTypeI <- get UI.selection outputType
    let oType = fromMaybe "" (snd . (outputTypes !!) <$> oTypeI)
    let preset = fromMaybe "" (snd . ((outputPresetsOfType Map.! oType) !!) <$> presetI)
    if preset == ""
      then element outputField # set UI.enabled False
                               # set UI.value ""
      else element outputField # set UI.enabled True
    return ()
  return ()

data State = State
  { filenames :: [String]
  , eMoved :: Event MidiControlState
  , hMoved :: Handler MidiControlState
  , eBankSwitch :: Event ()
  , hBankSwitch :: Handler ()
  , selecting :: IORef Bool
  , mappings :: IORef (Array Int Mapping)
  , currentMappingIndex :: IORef Int
  , buttonStates :: IORef ButtonStates
  }

mkState = (uncurry .) . uncurry . State

main :: IO ()
main = do
  either (error "Cannot initialize Midi") id <$> initialize
  (map (prettyDevice >=> putStrLn) . upTo) <$> countDevices >>= sequence

  state <- mkState <$> getArgs
                   <*> newEvent
                   <*> newEvent
                   <*> newIORef False
                   <*> (newIORef =<< mkArray <$> (sequence . map open =<< getArgs))
                   <*> newIORef 0
                   <*> newIORef (Map.empty)
  writeIORef (buttonStates state)
    . Map.fromList
    . map (\(MidiButton n,_) -> (n, False))
    . filter (\c -> case c of (MidiButton _,_) -> True; _ -> False)
    . concat
    . map Map.toList
    . elems
    <$> readIORef (mappings state)

  forkIO $ startGUI defaultConfig
    { jsPort   = Just 8023
    } (buildPage state)

  device <- readLn :: IO Int
  stream <- either (error "Cannot open device") id <$> openInput device

  processEvents stream state

