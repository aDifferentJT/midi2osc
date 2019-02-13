module Feedback (addFeedback, clearFeedback, addAllFeedbacks, clearAllFeedbacks) where

import Core
import Midi
import OSC
import OutputCore
import Utils

import Prelude hiding (sequence)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import Data.Map.Strict (Map, empty, findWithDefault, insert, delete)
import qualified Data.Map.Strict as Map (foldr, mapWithKey)
import Data.Traversable (sequence)
import System.IO.Unsafe (unsafePerformIO)

unRegFeedbacks :: IORef (Map MidiControl (IO ()))
unRegFeedbacks = unsafePerformIO (newIORef empty)

registerFeedback :: (Float -> IO ()) -> Output -> IO (IO ())
registerFeedback f (OSC inverted path) = forkIO (getOSCFeedback f inverted path)
                                     >>= return . killThread
registerFeedback _  _                  = return (return ())

getOSCFeedback :: (Float -> IO ()) -> Bool -> String -> IO ()
getOSCFeedback f inverted path = getOSCValue path ((>> (threadDelay 500000 >> getOSCFeedback f inverted path)) . f . invert inverted)

addFeedback :: PMStream -> MidiControl -> Output -> IO ()
addFeedback streamFb control output = do
  unRegOld <- findWithDefault (return ()) control <$> readIORef unRegFeedbacks
  unRegOld
  unRegNew <- registerFeedback (midiFeedbackFromFloat streamFb control) output
  modifyIORef' unRegFeedbacks (insert control unRegNew)

clearFeedback :: MidiControl -> IO ()
clearFeedback control = do
  unRegOld <- findWithDefault (return ()) control <$> readIORef unRegFeedbacks
  unRegOld
  modifyIORef' unRegFeedbacks (delete control)

clearAllFeedbacks :: IO ()
clearAllFeedbacks = do
  Map.foldr (>>) (return ()) =<< readIORef unRegFeedbacks
  writeIORef unRegFeedbacks empty

addAllFeedbacks :: State -> PMStream -> IO ()
addAllFeedbacks state streamFb = do
  clearAllFeedbacks
  currentMapping state
    >>= sequence . Map.mapWithKey (registerFeedback . midiFeedbackFromFloat streamFb)
    >>= writeIORef unRegFeedbacks

