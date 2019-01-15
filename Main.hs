import Sound.PortMidi
import Control.Monad
import Control.Concurrent
import Data.List
import Data.Word
import Data.Tuple
import Data.Maybe
import Data.IORef
import Text.Printf

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Reactive.Threepenny

upTo :: Int -> [Int]
upTo n = [0..n-1]

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x, y) = (x, f y)

bytes :: Integral a => a -> [Word8]
bytes = unfoldr f
  where f 0 = Nothing
        f n = Just . mapFst fromIntegral . swap . divMod n $ 256

prettyDevice :: DeviceID -> IO String
prettyDevice id = f <$> getDeviceInfo id
  where f (DeviceInfo _ nm True  False _) = show id ++ ": " ++ nm ++ " - Input"
        f (DeviceInfo _ nm False True  _) = show id ++ ": " ++ nm ++ " - Output"
        f (DeviceInfo _ nm True  True  _) = show id ++ ": " ++ nm ++ " - Input/Output"

data MidiControl = MidiButton Word8 Bool | MidiFader Word8 Word8 | MidiUnknown
  deriving Show

processEvents :: PMStream -> Handler MidiControl -> IORef Bool -> IO ()
processEvents stream hMoved selecting = do
  events <- either (error "Cannot get events") id <$> readEvents stream
  let controls = map (getControl . bytes . message) $ events
  selectingB <- readIORef selecting
  if selectingB && (not . null $ controls) then writeIORef selecting False >> (hMoved . last $ controls) else return ()
  processEvents stream hMoved selecting

printMessage :: [Word8] -> IO ()
printMessage = putStrLn . unwords . map (printf "0x%02x")

getControl :: [Word8] -> MidiControl
getControl ([0x90,n,0x7F]) = MidiButton n True
getControl ([0x80,n,0x7F]) = MidiButton n False
getControl ([0xB0,n     ]) = MidiFader n 0
getControl ([0xB0,n,l   ]) = MidiFader n l
getControl   _             = MidiUnknown

main :: IO ()
main = do
  either (error "Cannot initialize Midi") id <$> initialize
  (map (prettyDevice >=> putStrLn) . upTo) <$> countDevices >>= sequence

  (eMoved, hMoved) <- newEvent
  selecting <- newIORef False

  forkIO $ startGUI defaultConfig
    { jsPort   = Just 8023
    } $ \win -> do
      return win # set UI.title "midi2osc"
      button <- UI.button # set UI.text "Select control"
                          # set UI.style [("border-style","outset")]
      label <- UI.label # set UI.text "Please select a control first"
      getBody win #+ [element button, element label]
      on UI.click button $ const $ do
        element button # set UI.style [("border-style","inset")]
        liftIO (writeIORef selecting True)
      unregMoved <- liftIO $ register eMoved $ \control -> runUI win $ do
        element button # set UI.style [("border-style","outset")]
        element label  # set UI.text ("Control: " ++ case control of
            MidiButton n _ -> "Button " ++ show n
            MidiFader  n _ -> "Fader "  ++ show n
            MidiUnknown    -> "Unknown"
          )
        return ()
      return ()

  device <- readLn :: IO Int
  stream <- either (error "Cannot open device") id <$> openInput device

  processEvents stream hMoved selecting

