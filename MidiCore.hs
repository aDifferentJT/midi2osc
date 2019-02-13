{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module MidiCore (MidiControl (MidiButton, MidiFader, MidiUnknown), MidiControlState (MidiButtonState, MidiFaderState, MidiUnknownState), midiControlFromState, MidiButtonId (MidiButtonId), MidiFaderId (MidiFaderId), MidiButtonValue (MidiButtonValue), buttonValueMap, MidiFaderValue (MidiFaderValue)) where

import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Text.Printf (printf)

newtype MidiButtonId = MidiButtonId Word8
  deriving (Eq, Ord, Generic)
instance Serialize MidiButtonId

newtype MidiFaderId = MidiFaderId Word8
  deriving (Eq, Ord, Generic)
instance Serialize MidiFaderId

newtype MidiButtonValue = MidiButtonValue Bool
  deriving (Eq, Ord, Generic)
instance Serialize MidiButtonValue

buttonValueMap :: (Bool -> Bool) -> MidiButtonValue -> MidiButtonValue
buttonValueMap f (MidiButtonValue v) = MidiButtonValue (f v)

newtype MidiFaderValue = MidiFaderValue Word8
  deriving (Eq, Ord, Generic)
instance Serialize MidiFaderValue

data MidiControl = MidiButton MidiButtonId | MidiFader MidiFaderId | MidiUnknown
  deriving (Eq, Ord, Generic)
instance Serialize MidiControl

data MidiControlState = MidiButtonState MidiButtonId MidiButtonValue | MidiFaderState MidiFaderId MidiFaderValue | MidiUnknownState
  deriving (Eq, Ord, Generic)
instance Serialize MidiControlState

midiControlFromState :: MidiControlState -> MidiControl
midiControlFromState (MidiButtonState n _) = MidiButton n
midiControlFromState (MidiFaderState  n _) = MidiFader n
midiControlFromState  MidiUnknownState     = MidiUnknown

instance Show MidiControl where
  show (MidiButton (MidiButtonId n)) = printf "Button: %03d" n
  show (MidiFader  (MidiFaderId  n)) = printf "Fader:  %03d" n
  show  MidiUnknown                  = "Unknown"

instance Show MidiControlState where
  show (MidiButtonState (MidiButtonId n) (MidiButtonValue v)) = printf "Button: %03d  Level: %s" n (show v)
  show (MidiFaderState  (MidiFaderId  n) (MidiFaderValue  v)) = printf "Fader:  %03d  Level: %03d" n v
  show  MidiUnknownState                                      = "Unknown"

