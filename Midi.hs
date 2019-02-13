{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module Midi (MidiControl (MidiButton), MidiControlState, midiControlFromState, midiValueFromState, midiButtonValueFromFloat, midiInitialise, processEvents, openInput, openOutput, setLed, allLeds, midiFeedback, midiFeedbackFromFloat, PMStream) where

import MidiCore
import Utils

import Prelude hiding (lookup)
import Control.Monad ((>=>))
import Data.Bool (bool)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map, findWithDefault, alter, empty, insert)
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

import Sound.PortMidi

type MidiCallback = MidiControlState -> IO ()

midiInitialise :: IO ()
midiInitialise = do
  either (error "Cannot initialize Midi") id <$> initialize
  (map (prettyDevice >=> putStrLn) . upTo) <$> countDevices >>= sequence
  return ()

processEvents :: MidiCallback -> PMStream -> IO ()
processEvents callback stream = do
  sequence =<< map ((>>= callback) . getControl . bytes . message) . either (error "Cannot get events") id <$> readEvents stream
  processEvents callback stream

prettyDevice :: DeviceID -> IO String
prettyDevice id = f <$> getDeviceInfo id
  where f (DeviceInfo _ nm True  False _) = show id ++ ": " ++ nm ++ " - Input"
        f (DeviceInfo _ nm False True  _) = show id ++ ": " ++ nm ++ " - Output"
        f (DeviceInfo _ nm True  True  _) = show id ++ ": " ++ nm ++ " - Input/Output"

midiValueFromState :: MidiControlState -> Float
midiValueFromState (MidiButtonState _ (MidiButtonValue v)) = bool 0.0 1.0 v
midiValueFromState (MidiFaderState  _ (MidiFaderValue  v)) = (fromIntegral v) / 127.0
midiValueFromState  MidiUnknownState   = 0.0

midiButtonValueFromFloat :: Float -> MidiButtonValue
midiButtonValueFromFloat = MidiButtonValue . (/= 0.0)

printMessage :: [Word8] -> IO ()
printMessage = putStrLn . unwords . map (printf "0x%02x")

getControl :: [Word8] -> IO MidiControlState
getControl ([0x90,n',0x7F]) = let n = MidiButtonId n' in
                              flipStateOfButton n
                           >> MidiButtonState n <$> stateOfButton n
getControl ([0x80,n',0x7F]) = let n = MidiButtonId n' in
                              MidiButtonState n <$> stateOfButton n
getControl ([0xB0,n      ]) = return $ MidiFaderState (MidiFaderId n) (MidiFaderValue 0)
getControl ([0xB0,n,l    ]) = return $ MidiFaderState (MidiFaderId n) (MidiFaderValue l)
getControl   _              = return $ MidiUnknownState

type ButtonStates = Map MidiButtonId MidiButtonValue

buttonStates :: IORef ButtonStates
buttonStates = unsafePerformIO (newIORef empty)

stateOfButton :: MidiButtonId -> IO MidiButtonValue
stateOfButton n = findWithDefault (MidiButtonValue False) n <$> readIORef buttonStates

flipStateOfButton :: MidiButtonId -> IO ()
flipStateOfButton n = modifyIORef' buttonStates (alter (Just . maybe (MidiButtonValue False) (buttonValueMap not)) n)

ledMessage :: Word8 -> Bool -> [Word8]
ledMessage n b = [0x90, n, bool 0x00 0x7F b]

setLed :: PMStream -> MidiButtonId -> MidiButtonValue -> IO (Either PMError PMSuccess)
setLed stream (MidiButtonId n) (MidiButtonValue b) = writeEvents stream [PMEvent (unbytes (ledMessage n b)) 0]

allLeds :: [MidiButtonId]
allLeds = map MidiButtonId [minBound :: Word8 .. maxBound :: Word8]

performFeedback :: PMStream -> MidiButtonId -> MidiButtonValue -> IO ()
performFeedback streamFb n v = setLed streamFb n v
                            >> modifyIORef' buttonStates (insert n v)
                            >> return ()

midiFeedback :: PMStream -> MidiControlState -> IO ()
midiFeedback streamFb (MidiButtonState n v) = performFeedback streamFb n v
midiFeedback _        (_                  ) = return ()

midiFeedbackFromFloat :: PMStream -> MidiControl -> Float -> IO ()
midiFeedbackFromFloat streamFb (MidiButton n) = performFeedback streamFb n . midiButtonValueFromFloat
midiFeedbackFromFloat _         _             = const (return ())

