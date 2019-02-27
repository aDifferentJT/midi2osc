{-# LANGUAGE RecordWildCards #-}

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

import Control.Concurrent (threadDelay)
import Control.Concurrent.Lock (Lock, acquire, release)
import qualified Control.Concurrent.Lock as Lock (new)
import Control.Monad (void, when)
import Data.Array (bounds)
import Data.Bool (bool)
import Data.Bimap (lookupR)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map, findWithDefault, alter, empty, insert)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set, member)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)

import Sound.PortMidi

type ControlCallback = ControlState -> IO ()

midiState :: State -> MidiId -> MidiValue -> ControlState
midiState State{..} (MidiId n) (MidiButtonValue v) = ControlState (fromMaybe (error ("Button " ++ show n ++ " missing from profile")) . lookup (MidiButton . MidiId $ n) $ profile) (MidiButtonValue v)
midiState State{..} (MidiId n) (MidiFaderValue  v) = ControlState (fromMaybe (error ("Fader " ++ show n ++ " missing from profile")) . lookup (MidiFader . MidiId $ n) $ profile) (MidiFaderValue v)

processEvents :: State -> ControlCallback -> IO ()
processEvents State{..} callback = infLoop $ do
  startTime <- getCurrentTime
  sequence_ =<< map ((>>= callback) . getControl State{..} . bytes . message) . either (error "Cannot get events") id <$> readEvents stream
  endTime <- getCurrentTime
  threadDelay . truncate $ 1000000 * pollRate - diffUTCTime endTime startTime
  return ()

midiValueFromState :: ControlState -> Float
midiValueFromState (ControlState _ (MidiButtonValue v)) = bool 0.0 1.0 v
midiValueFromState (ControlState _ (MidiFaderValue  v)) = fromIntegral v / 127.0

midiButtonValueFromFloat :: Float -> MidiValue
midiButtonValueFromFloat = MidiButtonValue . (/= 0.0)

getControl :: State -> [Word8] -> IO ControlState
getControl State{..} [0x90,n',0x7F] = do
  let n = MidiId n'
  let c = fromMaybe (error ("Button " ++ show n' ++ " missing from profile")) . lookup (MidiButton n) $ profile
  if member c bankLefts || member c bankRights then
    return . ControlState c $ MidiButtonValue True
  else do
    flipStateOfButton n
    ControlState c <$> stateOfButton n
getControl State{..} [0x80,n',0x7F] = do
  let n = MidiId n'
  let c = fromMaybe (error ("Button " ++ show n' ++ " missing from profile")) . lookup (MidiButton n) $ profile
  if member c bankLefts || member c bankRights then
    return . ControlState c $ MidiButtonValue False
  else ControlState c <$> stateOfButton n
getControl state [0xB0,n      ] = return $ midiState state (MidiId n) (MidiFaderValue 0)
getControl state [0xB0,n,l    ] = return $ midiState state (MidiId n) (MidiFaderValue l)
getControl _      _             = fail "Unknown Midi Message"

type ButtonStates = Map MidiId MidiValue

{-# NOINLINE buttonStates #-}
buttonStates :: IORef ButtonStates
buttonStates = unsafePerformIO (newIORef empty)

stateOfButton :: MidiId -> IO MidiValue
stateOfButton n = findWithDefault (MidiButtonValue False) n <$> readIORef buttonStates

flipStateOfButton :: MidiId -> IO ()
flipStateOfButton n = modifyIORef' buttonStates (alter (Just . maybe (MidiButtonValue False) (buttonValueMap not)) n)

ledMessage :: Word8 -> Bool -> [Word8]
ledMessage n b = [0x90, n, bool 0x00 0x7F b]

{-# NOINLINE feedbackLock #-}
feedbackLock :: Lock
feedbackLock = unsafePerformIO Lock.new

setLed :: State -> Control -> MidiValue -> IO (Either PMError PMSuccess)
setLed State{..} control (MidiButtonValue v) = case lookupR control profile of
  Just (MidiButton (MidiId n)) -> do
    acquire feedbackLock
    r <- writeEvents streamFb [PMEvent (unbytes (ledMessage n v)) 0]
    release feedbackLock
    return r
  _ -> return $ Right NoError'NoData
setLed _     _        _                  = return $ Right NoError'NoData

setLeds :: State -> MidiValue -> Set Control -> IO ()
setLeds state v = mapM_ (void . \c -> setLed state c v) . toList

allLeds :: State -> Set Control
allLeds State{..} = fromList . mapMaybe f . toList $ profile
  where f :: (MidiControl, Control) -> Maybe Control
        f (MidiButton _, c) = Just c
        f  _                = Nothing

performFeedback :: State -> Control -> MidiValue -> IO ()
performFeedback State{..} c v = case fromMaybe (error ("Control " ++ show c ++ " missing from profile")) . lookupR c $ profile of
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
  if i > l then setLeds State{..} (MidiButtonValue True) bankLefts
  else          setLeds State{..} (MidiButtonValue False) bankLefts
  if i < r then setLeds State{..} (MidiButtonValue True) bankRights
  else          setLeds State{..} (MidiButtonValue False) bankRights

bankLeft :: State -> IO ()
bankLeft State{..} = do
  i <- readIORef currentMappingIndex
  (l,_) <- bounds <$> readIORef mappings
  when (i > l) $ do
    modifyIORef' currentMappingIndex (subtract 1)
    hBankSwitch ()
    return ()
  updateBankLeds State{..}

bankRight :: State -> IO ()
bankRight State{..} = do
  i <- readIORef currentMappingIndex
  (_,r) <- bounds <$> readIORef mappings
  when (i < r) $ do
    modifyIORef' currentMappingIndex (+ 1)
    hBankSwitch ()
    return ()
  updateBankLeds State{..}

