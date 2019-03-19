{-# LANGUAGE RecordWildCards, LambdaCase, DeriveAnyClass #-}

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
import Control.Exception (Exception, throw, throwIO)
import Control.Monad (void, when, unless, forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
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

data MidiException = MidiControlMissingFromProfileException MidiControl
                   | ControlMissingFromProfileException Control
                   | EventReadingException
                   | UnknownMidiMessageException
  deriving (Show, Exception)

lookupMidiControl :: State -> MidiControl -> Control
lookupMidiControl State{..} c = fromMaybe (throw (MidiControlMissingFromProfileException c)) . lookup c $ profile

lookupControl :: State -> Control -> MidiControl
lookupControl State{..} c = fromMaybe (throw (ControlMissingFromProfileException c)) . lookupR c $ profile

{-
midiState :: State -> MidiId -> MidiValue -> ControlState
midiState state n (MidiButtonValue v) = ControlState (lookupMidiControl state c) (MidiButtonValue v)
  where c = MidiButton n
midiState state n (MidiFaderValue  v) = ControlState (lookupMidiControl state c) (MidiFaderValue v)
  where c = MidiFader n
midiState _     _  MidiUnknownValue   = throw (MidiControlMissingFromProfileException MidiUnknown)
-}

processEvents :: State -> ControlCallback -> IO ()
processEvents State{..} callback = forever $ do
  startTime <- getCurrentTime
  mapM_ ((>>= callback) . getControl State{..} . bytes . message) =<< either (const . throwIO $ EventReadingException) return =<< readEvents stream
  endTime <- getCurrentTime
  threadDelay . truncate $ 1000000 * pollRate - diffUTCTime endTime startTime
  return ()

midiValueFromState :: ControlState -> Float
midiValueFromState (ControlState _ (MidiButtonValue v)) = bool 0.0 1.0 v
midiValueFromState (ControlState _ (MidiFaderValue  v)) = fromIntegral v / 127.0
midiValueFromState (ControlState _  MidiUnknownValue  ) = 0.0

midiButtonValueFromFloat :: Float -> MidiValue
midiButtonValueFromFloat = MidiButtonValue . (/= 0.0)

midiFaderValueFromFloat :: Float -> MidiValue
midiFaderValueFromFloat = MidiFaderValue . round . (* 127.0)

getButton :: State -> Word8 -> Bool -> IO ControlState
getButton State{..} nW8 v = do
  let n = MidiId nW8
  let c = lookupMidiControl State{..} (MidiButton n)
  if member c bankLefts || member c bankRights then
    return . ControlState c . MidiButtonValue $ v
  else do
    when v $ flipStateOfButton n
    ControlState c <$> stateOfButton n

getFader :: State -> Word8 -> Word8 -> IO ControlState
getFader State{..} nW8 vMovW8 = do
  let n = MidiId nW8
  let vMov = MidiFaderValue vMovW8
  let c = lookupMidiControl State{..} (MidiFader n)
  cache <- (\case
    Just (Recieved            vRec)
      | abs (vMov - vRec) < 2    -> Moved vMov
      | otherwise                -> MovedAndRecieved vMov vRec
    Just (Moved               _) -> Moved vMov
    Just (MovedAndRecieved vMov' vRec)
      | inBounds vMov vRec vMov' -> Moved vMov
      | otherwise                -> MovedAndRecieved vMov vRec
    Nothing                      -> Moved vMov
    ) . lookup n <$> readIORef faderCatches
  modifyIORef' faderCatches (insert n cache)
  let caught = case cache of
        Recieved         _   -> True
        Moved            _   -> False
        MovedAndRecieved _ _ -> True
  unless caught . runMaybeT_ $ do
    fb <- MaybeT . return $ lookup c fbProfile
    _ <- lift $ setLed State{..} fb (MidiButtonValue False)
    return ()
  return . ControlState c $ case cache of
    Recieved v           -> v
    Moved v              -> v
    MovedAndRecieved _ v -> v
  where inBounds :: Ord a => a -> a -> a -> Bool
        inBounds x y z = (x <= y && y <= z) || (x >= y && y >= z)

getControl :: State -> [Word8] -> IO ControlState
getControl state [0x90,n,0x7F] = getButton state n True
getControl state [0x80,n,0x7F] = getButton state n False
getControl state [0xB0,n     ] = getFader  state n 0 --return $ midiState state (MidiId n) (MidiFaderValue 0)
getControl state [0xB0,n,l   ] = getFader  state n l --return $ midiState state (MidiId n) (MidiFaderValue l)
getControl _      _            = throwIO UnknownMidiMessageException

type ButtonStates = Map MidiId MidiValue

{-# NOINLINE buttonStates #-}
buttonStates :: IORef ButtonStates
buttonStates = unsafePerformIO (newIORef empty)

stateOfButton :: MidiId -> IO MidiValue
stateOfButton n = findWithDefault (MidiButtonValue False) n <$> readIORef buttonStates

flipStateOfButton :: MidiId -> IO ()
flipStateOfButton n = modifyIORef' buttonStates (alter (Just . maybe (MidiButtonValue False) (buttonValueMap not)) n)

data FaderState   = Recieved MidiValue | Moved MidiValue | MovedAndRecieved MidiValue MidiValue
  deriving (Show)
type FaderCatches = Map MidiId FaderState

{-# NOINLINE faderCatches #-}
faderCatches :: IORef FaderCatches
faderCatches = unsafePerformIO (newIORef empty)

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
performFeedback State{..} c v = case lookupControl State{..} c of
  MidiButton n -> do
    r <- setLed State{..} c v
    case r of
      Left  e -> putStrLn =<< getErrorText e
      Right _ -> return ()
    modifyIORef' buttonStates (insert n v)
    return ()
  MidiFader n -> do
    modifyIORef' faderCatches (alter (Just . \case
      Nothing                      -> Recieved v
      Just (Recieved            _) -> Recieved v
      Just (Moved            v'  )
        | v' == v                  -> Moved v'
        | otherwise                -> MovedAndRecieved v' v
      Just (MovedAndRecieved v' _)
        | v' == v                  -> Moved v'
        | otherwise                -> MovedAndRecieved v' v
      ) n)
    caught <- (\case
      Nothing                     -> False
      Just (Recieved         _  ) -> True
      Just (Moved            _  ) -> False
      Just (MovedAndRecieved _ _) -> True
      ) . lookup n <$> readIORef faderCatches
    when caught . runMaybeT_ $ do
      fb <- MaybeT . return $ lookup c fbProfile
      _ <- lift $ setLed State{..} fb (MidiButtonValue True)
      return ()
  MidiUnknown -> return ()

midiFeedback :: State -> ControlState -> IO ()
midiFeedback state (ControlState c v) = performFeedback state c v

midiFeedbackFromFloat :: State -> Control -> Float -> IO ()
midiFeedbackFromFloat state c = performFeedback state c . case lookupControl state c of
  MidiButton _ -> midiButtonValueFromFloat
  MidiFader  _ -> midiFaderValueFromFloat
  MidiUnknown  -> const MidiUnknownValue

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

