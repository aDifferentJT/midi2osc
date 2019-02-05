{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module Midi (MidiControl (MidiButton), MidiControlState, midiControlFromState, midiValueFromState, midiInitialise, processEvents, openInput) where

import Core
import MidiCore
import Utils

import Prelude hiding (lookup)
import Control.Monad
import Data.Bool
import Data.IORef
import Data.Map (lookup, update)
import Data.Maybe
import qualified Data.Serialize as Serialize
import Data.Word
import GHC.Generics
import Text.Printf

import Sound.PortMidi

type MidiCallback = MidiControlState -> IO ()

midiInitialise :: IO ()
midiInitialise = do
  either (error "Cannot initialize Midi") id <$> initialize
  (map (prettyDevice >=> putStrLn) . upTo) <$> countDevices >>= sequence
  return ()

processEvents :: MidiCallback -> PMStream -> IO ()
processEvents callback stream = do
  sequence =<< map (callback . getControl . bytes . message) . either (error "Cannot get events") id <$> readEvents stream
  processEvents callback stream

prettyDevice :: DeviceID -> IO String
prettyDevice id = f <$> getDeviceInfo id
  where f (DeviceInfo _ nm True  False _) = show id ++ ": " ++ nm ++ " - Input"
        f (DeviceInfo _ nm False True  _) = show id ++ ": " ++ nm ++ " - Output"
        f (DeviceInfo _ nm True  True  _) = show id ++ ": " ++ nm ++ " - Input/Output"

midiValueFromState :: State -> MidiControlState -> IO Float
midiValueFromState state (MidiButtonState n True ) = modifyIORef' (buttonStates state) (update (Just . not) n)
                                                  >> bool 0.0 1.0 . fromMaybe False . lookup n <$> readIORef (buttonStates state)
midiValueFromState state (MidiButtonState n False) = bool 0.0 1.0 . fromMaybe False . lookup n <$> readIORef (buttonStates state)
midiValueFromState _     (MidiFaderState  _ v    ) = return ((fromIntegral v) / 127.0)
midiValueFromState _      MidiUnknownState         = return 0.0

printMessage :: [Word8] -> IO ()
printMessage = putStrLn . unwords . map (printf "0x%02x")

getControl :: [Word8] -> MidiControlState
getControl ([0x90,n,0x7F]) = MidiButtonState n True
getControl ([0x80,n,0x7F]) = MidiButtonState n False
getControl ([0xB0,n     ]) = MidiFaderState n 0
getControl ([0xB0,n,l   ]) = MidiFaderState n l
getControl   _             = MidiUnknownState

