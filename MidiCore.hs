{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module MidiCore
  ( MidiControl (MidiButton, MidiFader, MidiUnknown)
  , MidiId (MidiId)
  , MidiValue (MidiButtonValue, MidiFaderValue, MidiUnknownValue)
  , buttonValueMap
  , faderValueMap
  , Control (Control)
  , ControlState (ControlState)
  , openDevice
  ) where

import Control.Monad ((<=<), when, filterM)
import Control.Exception (Exception, throwIO)
import Data.Bool (bool)
import Data.List (isPrefixOf)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)

import Sound.PortMidi

data MidiException = MidiInitialisationException
                   | DeviceNotFoundException String
                   | InputDeviceNotFoundException String
                   | OutputDeviceNotFoundException String
                   | InputDeviceOpeningException DeviceID
                   | OutputDeviceOpeningException DeviceID
  deriving (Show, Exception)

newtype MidiId = MidiId Word8
  deriving (Show, Eq, Ord, Generic)
instance Serialize MidiId

data MidiValue = MidiButtonValue Bool | MidiFaderValue Word8 | MidiUnknownValue
  deriving (Eq, Ord, Generic)
instance Serialize MidiValue
instance Show MidiValue where
  show (MidiButtonValue v) = show v
  show (MidiFaderValue  v) = padLeft 3 '0' (show v)
  show  MidiUnknownValue   = "Unknown"
instance Num MidiValue where
  MidiButtonValue v1 + MidiButtonValue v2                         = MidiButtonValue (v1 || v2)
  MidiFaderValue  v1 + MidiFaderValue  v2
    | toInteger v1 + toInteger v2 > toInteger (maxBound :: Word8) = MidiFaderValue maxBound
    | otherwise                                                   = MidiFaderValue (v1 + v2)
  _                  + _                                          = MidiUnknownValue
  MidiButtonValue v1 * MidiButtonValue v2                         = MidiButtonValue (v1 && v2)
  MidiFaderValue  v1 * MidiFaderValue  v2
    | toInteger v1 * toInteger v2 > toInteger (maxBound :: Word8) = MidiFaderValue maxBound
    | otherwise                                                   = MidiFaderValue (v1 * v2)
  _                  * _                                          = MidiUnknownValue
  abs                        = id
  signum (MidiButtonValue v) = bool 0 1 v
  signum (MidiFaderValue  0) = 0
  signum (MidiFaderValue  _) = 1
  signum  MidiUnknownValue   = 0
  MidiButtonValue v1 - MidiButtonValue v2 = MidiButtonValue (xor v1 v2)
    where xor :: Bool -> Bool -> Bool
          xor False False = False
          xor False True  = True
          xor True  False = True
          xor True  True  = False
  MidiFaderValue  v1 - MidiFaderValue  v2 = MidiFaderValue (abs (v1 - v2))
  _                  - _                  = MidiUnknownValue
  fromInteger n
    | n > toInteger (maxBound :: Word8) = MidiFaderValue maxBound
    | n < toInteger (minBound :: Word8) = MidiFaderValue minBound
    | otherwise                         = MidiFaderValue . fromInteger $ n

buttonValueMap :: (Bool -> Bool) -> MidiValue -> MidiValue
buttonValueMap f (MidiButtonValue v) = MidiButtonValue (f v)
buttonValueMap _ (MidiFaderValue  v) = MidiFaderValue v
buttonValueMap _  MidiUnknownValue   = MidiUnknownValue

faderValueMap :: (Word8 -> Word8) -> MidiValue -> MidiValue
faderValueMap f (MidiFaderValue v) = MidiFaderValue (f v)
faderValueMap _ (MidiButtonValue  v) = MidiButtonValue v
faderValueMap _  MidiUnknownValue   = MidiUnknownValue

data MidiControl = MidiButton MidiId | MidiFader MidiId | MidiUnknown
  deriving (Eq, Ord, Generic)
instance Serialize MidiControl

instance Show MidiControl where
  show (MidiButton (MidiId n)) = "Button: " ++ padLeft 3 '0' (show n)
  show (MidiFader  (MidiId n)) = "Fader:  " ++ padLeft 3 '0' (show n)
  show  MidiUnknown            = "Unknown"

newtype Control = Control String
  deriving (Eq, Ord, Generic)
instance Serialize Control
instance Show Control where
  show (Control s) = s

data ControlState = ControlState Control MidiValue
instance Show ControlState where
  show (ControlState c v) = show c ++ ": " ++ show v

openDevice :: String -> IO (PMStream, PMStream)
openDevice d = do
  _ <- either (const . throwIO $ MidiInitialisationException) return =<< initialize
  devices <- filterM (return . isPrefixOf d . name <=< getDeviceInfo) . upTo =<< countDevices
  when (null devices) $ throwIO (DeviceNotFoundException d)
  inDevice <- maybe (throwIO (InputDeviceNotFoundException d)) return . head =<< filterM (return . input <=< getDeviceInfo) devices
  outDevice <- maybe (throwIO (OutputDeviceNotFoundException d)) return . head =<< filterM (return . output <=< getDeviceInfo) devices
  inStream <- either (const . throwIO . InputDeviceOpeningException $ inDevice) return =<< openInput inDevice
  outStream <- either (const . throwIO . OutputDeviceOpeningException $ outDevice) return =<< openOutput outDevice 0
  return (inStream, outStream)

