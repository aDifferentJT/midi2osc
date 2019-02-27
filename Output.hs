{-# LANGUAGE RecordWildCards #-}

module Output
  ( Output
  , outputTypes
  , outputTypeArgs
  , outputPresetsOfType
  , outputPresets
  , outputChannelPresetsOfType
  , outputChannelPresets
  , outputActionPresetsOfType
  , outputActionPresets
  , performOutput
  ) where

import Core
import Feedback ()
import OSC
import OutputCore

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict (Map, keys)

outputTypes :: [(String, String)]
outputTypes = [("OSC", "OSC")]

outputTypeArgs :: State -> Map String [(String, String)]
outputTypeArgs State{..} = fromList
  [ ("OSC", map (\(Connection s) -> (s,s)) . keys $ oscConnections)
  ]

outputPresetsOfType :: Map String [(String, String)]
outputPresetsOfType = fromList
  [ ("OSC",
    [ ("Other", "OSCother")
    , ("Other (Inverted)", "OSCotherInverted")
    ])
  ]

outputPresets :: Map String (Factory String (Factory String Output))
outputPresets = fromList
  [ ("OSCother", Function (\c -> Function (OSC (Connection c) False)))
  , ("OSCotherInverted", Function (\c -> Function (OSC (Connection c) True)))
  ]

outputChannelPresetsOfType :: Map String [(String, String)]
outputChannelPresetsOfType = fromList
  [ ("OSC",
    [ ("Input", "OSCinput")
    , ("Aux", "OSCaux")
    , ("LR"  , "OSClr"  )
    ])
  ]

outputChannelPresets :: Map String (Factory String (Factory String OutputChannel))
outputChannelPresets = fromList
  [ ("OSCinput", Function (\c -> Function (OSCInput (Connection c) . (read :: String -> Int))))
  , ("OSCaux", Function $ Value . OSCAux . Connection)
  , ("OSClr",  Function $ Value . OSCLR  . Connection)
  ]

outputActionPresetsOfType :: Map String [(String, String)]
outputActionPresetsOfType = fromList
  [ ("OSC",
    [ ("Fade", "OSCfade")
    , ("Mute", "OSCmute")
    , ("On"  , "OSCon"  )
    , ("Gain", "OSCgain")
    ])
  ]

outputActionPresets :: Map String (Factory String OutputAction)
outputActionPresets = fromList
  [ ("OSCfade", Value OSCFader)
  , ("OSCmute", Value OSCMute)
  , ("OSCon"  , Value OSCOn)
  , ("OSCgain", Value OSCGain)
  ]

performOutput :: MonadIO m => State -> (Output, Float) -> m ()
performOutput _         (Print x,     _) = liftIO $ print x
performOutput State{..} (OSC c inv p, v) = setOSCValue (oscConnections ! c) p (invert inv v)

