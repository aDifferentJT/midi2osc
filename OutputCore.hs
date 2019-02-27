{-# LANGUAGE DeriveGeneric #-}

module OutputCore
  ( Output (..)
  , OutputChannel (..)
  , OutputAction (..)
  , outputCombine
  ) where

import ConfParser

import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Output = Print String | OSC Connection Bool String
  deriving Generic
instance Serialize Output
instance Show Output where
  show (Print s)        = "Print " ++ s
  show (OSC c inv path) = if inv then "OSC " ++ show c ++ " (Inverted): " ++ path else "OSC " ++ show c ++ ": " ++ path

data OutputChannel = OSCInput Connection Int | OSCAux Connection | OSCLR Connection
  deriving (Show, Generic)
instance Serialize OutputChannel

data OutputAction = OSCFader | OSCMute | OSCOn | OSCGain
  deriving (Show, Generic)
instance Serialize OutputAction

outputCombine :: OutputChannel -> OutputAction -> Maybe Output
outputCombine (OSCInput c n) OSCFader = Just . OSC c False $ "/ch/" ++ padLeft 2 '0' (show n) ++ "/mix/fader"
outputCombine (OSCInput c n) OSCMute  = Just . OSC c True  $ "/ch/" ++ padLeft 2 '0' (show n) ++ "/mix/on"
outputCombine (OSCInput c n) OSCOn    = Just . OSC c False $ "/ch/" ++ padLeft 2 '0' (show n) ++ "/mix/on"
outputCombine (OSCInput c n) OSCGain  = Just . OSC c False $ "/headamp/" ++ padLeft 2 '0' (show n) ++ "/gain"
outputCombine (OSCAux c)     OSCFader = Just . OSC c False $ "/rtn/aux/mix/fader"
outputCombine (OSCAux c)     OSCMute  = Just . OSC c True  $ "/rtn/aux/mix/on"
outputCombine (OSCAux c)     OSCOn    = Just . OSC c False $ "/rtn/aux/mix/on"
outputCombine (OSCAux c)     OSCGain  = Just . OSC c False $ "/headamp/17/gain"
outputCombine (OSCLR c)      OSCFader = Just . OSC c False $ "/lr/mix/fader"
outputCombine (OSCLR c)      OSCMute  = Just . OSC c True  $ "/lr/mix/on"
outputCombine (OSCLR c)      OSCOn    = Just . OSC c False $ "/lr/mix/on"
outputCombine (OSCLR _)      OSCGain  = Nothing
--outputCombine  _             _        = Nothing

