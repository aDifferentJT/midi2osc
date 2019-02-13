{-# LANGUAGE DeriveGeneric, DefaultSignatures, TupleSections #-}

module Main (main) where

import Core
import Feedback
import GUI
import Midi
import MidiCore
import Output
import OutputCore ()
import Utils

import Prelude hiding (lookup)
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.IORef (readIORef)
import Data.Map.Strict (lookup)
import System.Environment (getArgs)

respondToControl :: State -> MidiControlState -> MaybeT IO (Output, Float)
respondToControl state control = do
  let v = midiValueFromState control
  o <- MaybeT . (lookup (midiControlFromState control) <$>) $ currentMapping state
  return (o, v)

eventCallback :: State -> PMStream -> MidiControlState -> IO ()
eventCallback state streamFb control = do
  selectingB <- readIORef . selecting $ state
  runMaybeT $ if not selectingB then respondToControl state control >>= performOutput state else nothingT
  hMoved state control
  midiFeedback streamFb control
  return ()

main :: IO ()
main = do
  midiInitialise

  state <- defaultState =<< getArgs

  device <- readLn :: IO Int
  stream <- either (error ("Cannot open input device " ++ show device)) id <$> openInput device

  deviceFb <- readLn :: IO Int
  streamFb <- either (error ("Cannot open output device " ++ show deviceFb)) id <$> openOutput deviceFb 0

  runGUI state streamFb

  addAllFeedbacks state streamFb

  threadDelay 500000
  sequence $ map (\n -> setLed streamFb n (MidiButtonValue False)) allLeds
  threadDelay 500000
  sequence $ map (\n -> setLed streamFb n (MidiButtonValue True)) allLeds
  threadDelay 500000
  sequence $ map (\n -> setLed streamFb n (MidiButtonValue False)) allLeds
  threadDelay 500000

  processEvents (eventCallback state streamFb) stream

