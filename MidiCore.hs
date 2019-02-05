{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module MidiCore (MidiControl (MidiButton, MidiFader, MidiUnknown), MidiControlState (MidiButtonState, MidiFaderState, MidiUnknownState), midiControlFromState) where
--module MidiCore (MidiControl, MidiControlState) where

import Data.Serialize (Serialize)
import Data.Word
import GHC.Generics
import Text.Printf

import Sound.PortMidi

data MidiControl = MidiButton Word8 | MidiFader Word8 | MidiUnknown
  deriving (Eq, Ord, Generic)

instance Serialize MidiControl

data MidiControlState = MidiButtonState Word8 Bool | MidiFaderState Word8 Word8 | MidiUnknownState
  deriving (Eq, Ord)

midiControlFromState :: MidiControlState -> MidiControl
midiControlFromState (MidiButtonState n _) = MidiButton n
midiControlFromState (MidiFaderState  n _) = MidiFader n
midiControlFromState  MidiUnknownState     = MidiUnknown

instance Show MidiControl where
  show (MidiButton n) = printf "Button: %03d" n
  show (MidiFader  n) = printf "Fader:  %03d" n
  show  MidiUnknown   = "Unknown"

instance Show MidiControlState where
  show (MidiButtonState n v) = printf "Button: %03d  Level: %s" n (show v)
  show (MidiFaderState  n v) = printf "Fader:  %03d  Level: %03d" n v
  show  MidiUnknownState     = "Unknown"

