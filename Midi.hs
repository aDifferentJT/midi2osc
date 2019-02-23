{-# LANGUAGE DeriveGeneric, DefaultSignatures, RecordWildCards #-}

module Midi
  ( MidiControl (MidiButton)
  , midiValueFromState
  , midiButtonValueFromFloat
  , processEvents
  , openInput
  , openOutput
  , setLed
  , setLeds
  , allLeds
  , midiFeedback
  , midiFeedbackFromFloat
  , updateBankLeds
  , bankLeft
  , bankRight
  , PMStream
  ) where

import Core
import MidiCore
import Utils

import Prelude hiding (lookup)
import Control.Concurrent.Lock (Lock, acquire, release)
import qualified Control.Concurrent.Lock as Lock (new)
import Control.Monad (void)
import Data.Array (bounds)
import Data.Bool (bool)
import Data.Bimap (lookup, lookupR)
import qualified Data.Bimap as Bimap (toList)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map, findWithDefault, alter, empty, insert)
import Data.Maybe (mapMaybe)
import Data.Set (Set, member)
import qualified Data.Set as Set (toList, fromList)
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

import Sound.PortMidi

type ControlCallback = ControlState -> IO ()

midiState :: State -> MidiId -> MidiValue -> ControlState
midiState State{..} (MidiId n) (MidiButtonValue v) = ControlState (maybe (error (printf "Button %i missing from profile" n)) id . lookup (MidiButton . MidiId $ n) $ profile) (MidiButtonValue v)
midiState State{..} (MidiId n) (MidiFaderValue  v) = ControlState (maybe (error (printf "Fader %i missing from profile" n)) id . lookup (MidiFader . MidiId $ n) $ profile) (MidiFaderValue v)

processEvents :: State -> ControlCallback -> IO ()
processEvents State{..} callback = infLoop $ do
  _ <- sequence =<< map ((>>= callback) . getControl State{..} . bytes . message) . either (error "Cannot get events") id <$> readEvents stream
  return ()

midiValueFromState :: ControlState -> Float
midiValueFromState (ControlState _ (MidiButtonValue v)) = bool 0.0 1.0 v
midiValueFromState (ControlState _ (MidiFaderValue  v)) = (fromIntegral v) / 127.0

midiButtonValueFromFloat :: Float -> MidiValue
midiButtonValueFromFloat = MidiButtonValue . (/= 0.0)

getControl :: State -> [Word8] -> IO ControlState
getControl State{..} ([0x90,n',0x7F]) = do
  let n = MidiId n'
  let c = maybe (error (printf "Button %i missing from profile" n')) id . lookup (MidiButton n) $ profile
  if (member c bankLefts) || (member c bankRights) then
    return (ControlState c (MidiButtonValue True))
  else do
    flipStateOfButton n
    ControlState c <$> stateOfButton n
getControl State{..} ([0x80,n',0x7F]) = do
  let n = MidiId n'
  let c = maybe (error (printf "Button %i missing from profile" n')) id . lookup (MidiButton n) $ profile
  if (member c bankLefts) || (member c bankRights) then
    return (ControlState c (MidiButtonValue False))
  else do
    ControlState c <$> stateOfButton n
getControl state ([0xB0,n      ]) = return $ midiState state (MidiId n) (MidiFaderValue 0)
getControl state ([0xB0,n,l    ]) = return $ midiState state (MidiId n) (MidiFaderValue l)
getControl _       _              = fail "Unknown Midi Message"

type ButtonStates = Map MidiId MidiValue

buttonStates :: IORef ButtonStates
buttonStates = unsafePerformIO (newIORef empty)

stateOfButton :: MidiId -> IO MidiValue
stateOfButton n = findWithDefault (MidiButtonValue False) n <$> readIORef buttonStates

flipStateOfButton :: MidiId -> IO ()
flipStateOfButton n = modifyIORef' buttonStates (alter (Just . maybe (MidiButtonValue False) (buttonValueMap not)) n)

ledMessage :: Word8 -> Bool -> [Word8]
ledMessage n b = [0x90, n, bool 0x00 0x7F b]

feedbackLock :: Lock
feedbackLock = unsafePerformIO Lock.new

setLed :: State -> Control -> MidiValue -> IO (Either PMError PMSuccess)
setLed State{..} control (MidiButtonValue v) = do
  case lookupR control profile of
    Just (MidiButton (MidiId n)) -> do
      acquire feedbackLock
      r <- writeEvents streamFb [PMEvent (unbytes (ledMessage n v)) 0]
      release feedbackLock
      return r
    _ -> do
      return (Right NoError'NoData)
setLed _     _        _                  = do
  return (Right NoError'NoData)

setLeds :: State -> MidiValue -> Set Control -> IO ()
setLeds state v = void . sequence . map (\c -> setLed state c v) . Set.toList

allLeds :: State -> Set Control
allLeds State{..} = Set.fromList . mapMaybe f . Bimap.toList $ profile
  where f :: (MidiControl, Control) -> Maybe Control
        f (MidiButton _, c) = Just c
        f  _                = Nothing

performFeedback :: State -> Control -> MidiValue -> IO ()
performFeedback State{..} c v = do
  case maybe (error (printf "Control %s missing from profile" . show $ c)) id . lookupR c $ profile of
    MidiButton n -> do
      r <- setLed State{..} c v
      case r of
        Left  e -> putStrLn =<< getErrorText e
        Right _ -> return ()
      modifyIORef' buttonStates (insert n v)
      return ()
    _ -> return ()

midiFeedback :: State -> ControlState -> IO ()
midiFeedback state (ControlState c v) = performFeedback state c v

midiFeedbackFromFloat :: State -> Control -> Float -> IO ()
midiFeedbackFromFloat state c = performFeedback state c . midiButtonValueFromFloat

updateBankLeds :: State -> IO ()
updateBankLeds State{..} = do
  i <- readIORef currentMappingIndex
  (l,r) <- bounds <$> readIORef mappings
  if i > l then do
    setLeds State{..} (MidiButtonValue True) bankLefts
  else do
    setLeds State{..} (MidiButtonValue False) bankLefts
  if i < r then do
    setLeds State{..} (MidiButtonValue True) bankRights
  else do
    setLeds State{..} (MidiButtonValue False) bankRights

bankLeft :: State -> IO ()
bankLeft State{..} = do
  i <- readIORef currentMappingIndex
  (l,_) <- bounds <$> readIORef mappings
  if i > l then do
    modifyIORef' currentMappingIndex (subtract 1)
    hBankSwitch ()
    return ()
  else return ()
  updateBankLeds State{..}

bankRight :: State -> IO ()
bankRight State{..} = do
  i <- readIORef currentMappingIndex
  (_,r) <- bounds <$> readIORef mappings
  if i < r then do
    modifyIORef' currentMappingIndex (+ 1)
    hBankSwitch ()
    return ()
  else return ()
  updateBankLeds State{..}
