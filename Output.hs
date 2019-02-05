module Output (Output, outputTypes, outputPresetsOfType, outputPresets, performOutput) where

import Core
import Midi
import OSC
import OutputCore
import qualified Sound.OSC as OSC

import Control.Monad.IO.Class
import Data.IORef
import Data.Map (Map, fromList)
import Data.Serialize (Serialize)
import GHC.Generics
import Text.Printf

outputTypes :: [(String, String)]
outputTypes = [("--", ""), ("OSC", "OSC"), ("Switch Bank", "BankSwitch")]

outputPresetsOfType :: Map String [(String, String)]
outputPresetsOfType = fromList
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

outputPresets :: Map String (String -> String)
outputPresets = fromList
  [ ("OSCfade", printf "\"/ch/%02d/mix/fader\"" . (read :: String -> Int))
  , ("OSCmute", printf "\"/ch/%02d/mix/on\"" . (read :: String -> Int))
  , ("OSCgain", printf "\"/headamp/%02d/gain\"" . (read :: String -> Int))
  , ("OSCother", ("\"" ++) . (++ "\""))
  , ("BankSwitch", id)
  ]

performOutput :: MonadIO m => State -> (Output, Float) -> m ()
performOutput _     (Print x,      _    ) = liftIO $ print x
performOutput state (OSC path,     value) = sendOSC . OSC.Message path . (:[]) . OSC.Float $ value
performOutput state (BankSwitch n, _    ) = liftIO $ writeIORef (currentMappingIndex state) n >> hBankSwitch state ()

