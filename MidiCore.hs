{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module MidiCore
  ( MidiControl (MidiButton, MidiFader, MidiUnknown)
  , MidiId (MidiId)
  , MidiValue (MidiButtonValue, MidiFaderValue)
  , buttonValueMap
  , faderValueMap
  , Control (Control)
  , ControlState (ControlState)
  , openDevice
  ) where

import Utils

import Control.Monad ((<=<), filterM)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Text.Printf (printf)

import Sound.PortMidi

newtype MidiId = MidiId Word8
  deriving (Show, Eq, Ord, Generic)
instance Serialize MidiId

data MidiValue = MidiButtonValue Bool | MidiFaderValue Word8
  deriving (Eq, Ord, Generic)
instance Serialize MidiValue
instance Show MidiValue where
  show (MidiButtonValue v) = show v
  show (MidiFaderValue  v) = printf "%03d" v

buttonValueMap :: (Bool -> Bool) -> MidiValue -> MidiValue
buttonValueMap f (MidiButtonValue v) = MidiButtonValue (f v)
buttonValueMap _ (MidiFaderValue  v) = MidiFaderValue v

faderValueMap :: (Word8 -> Word8) -> MidiValue -> MidiValue
faderValueMap f (MidiFaderValue v) = MidiFaderValue (f v)
faderValueMap _ (MidiButtonValue  v) = MidiButtonValue v

data MidiControl = MidiButton MidiId | MidiFader MidiId | MidiUnknown
  deriving (Eq, Ord, Generic)
instance Serialize MidiControl

instance Show MidiControl where
  show (MidiButton (MidiId n)) = printf "Button: %03d" n
  show (MidiFader  (MidiId n)) = printf "Fader:  %03d" n
  show  MidiUnknown                  = "Unknown"

newtype Control = Control String
  deriving (Eq, Ord, Generic)
instance Serialize Control
instance Show Control where
  show (Control s) = s

data ControlState = ControlState Control MidiValue
instance Show ControlState where
  show (ControlState c v) = printf "%s: %s" (show c) (show v)

openDevice :: String -> IO (PMStream, PMStream)
openDevice d = do
  _ <- either (error "Cannot initialize Midi") id <$> initialize
  devices <- filterM (return . (== d) . name <=< getDeviceInfo) . upTo =<< countDevices
  if null devices then error (printf "No device named %s" d) else return ()
  inDevice <- filterM (return . input <=< getDeviceInfo) devices
  outDevice <- filterM (return . output <=< getDeviceInfo) devices
  inStream <- either (error ("Cannot open input device " ++ show inDevice)) id <$> (openInput . head $ inDevice)
  outStream <- either (error ("Cannot open output device " ++ show outDevice)) id <$> ((flip openOutput 0) . head $ outDevice)
  return (inStream, outStream)

