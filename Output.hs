module Output (Output, outputTypes, outputPresetsOfType, outputPresets, performOutput, updateOutput) where

import Core
import Feedback
import Midi
import OSC
import OutputCore
import Utils

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Array as Array ((!))
import Data.Array ((//))
import Data.IORef (readIORef, writeIORef, modifyIORef')
import Data.Map.Strict (Map, fromList, insert)
import Text.Printf (printf)

outputTypes :: [(String, String)]
outputTypes = [("--", ""), ("OSC", "OSC"), ("Switch Bank", "BankSwitch")]

outputPresetsOfType :: Map String [(String, String)]
outputPresetsOfType = fromList
  [ ("OSC",
    [ ("--", "")
    , ("Fade ch", "OSCfade")
    , ("Mute ch", "OSCmute")
    , ("On ch"  , "OSCon"  )
    , ("Gain ch", "OSCgain")
    , ("Other", "OSCother")
    , ("Other (Inverted)", "OSCotherInverted")
    ])
  , ("BankSwitch",
    [ ("--", "")
    , ("number", "BankSwitch")
    ])
  ]

outputPresets :: Map String (String -> Output)
outputPresets = fromList
  [ ("OSCfade", OSC False . printf "/ch/%02d/mix/fader" . (read :: String -> Int))
  , ("OSCmute", OSC True  . printf "/ch/%02d/mix/on"    . (read :: String -> Int))
  , ("OSCon"  , OSC False . printf "/ch/%02d/mix/on"    . (read :: String -> Int))
  , ("OSCgain", OSC False . printf "/headamp/%02d/gain" . (read :: String -> Int))
  , ("OSCother", OSC False)
  , ("OSCotherInverted", OSC True)
  , ("BankSwitch", BankSwitch . read)
  ]

performOutput :: MonadIO m => State -> (Output, Float) -> m ()
performOutput _     (Print x,               _) = liftIO $ print x
performOutput _     (OSC inverted path,     v) = setOSCValue path (invert inverted v)
performOutput state (BankSwitch n,          _) = liftIO $ writeIORef (currentMappingIndex state) n >> hBankSwitch state ()

updateOutput :: State -> PMStream -> MidiControl -> Output -> IO ()
updateOutput state streamFb control newOutput = do
  i <- readIORef . currentMappingIndex $ state
  modifyIORef' (mappings state) (\ms -> ms // [(i, insert control newOutput (ms Array.! i))])
  currentMapping state >>= save (filenames state !! i)
  addFeedback streamFb control newOutput

