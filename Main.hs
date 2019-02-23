{-# LANGUAGE DeriveGeneric, DefaultSignatures, TupleSections, RecordWildCards #-}

module Main (main) where

import Core
import GUI
import Midi
import MidiCore
import Output
import Utils

import Prelude hiding (lookup)
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Set (member)
import System.Environment (getArgs)

import Reactive.Threepenny hiding (empty)

respondToControl :: State -> ControlState -> MaybeT IO (Output, Float)
respondToControl State{..} controlState = do
  let v = midiValueFromState controlState
  let ControlState control _ = controlState
  o <- MaybeT (outputForControl State{..} <$> currentMapping <*> return control)
  return (o, v)

eventCallback :: State -> ControlState -> IO ()
eventCallback State{..} (ControlState c (MidiButtonValue v))
  | member c bankLefts =
    if v then do
      bankLeft State{..}
      addAllFeedbacks State{..}
    else return ()
  | member c bankRights =
    if v then do
      bankRight State{..}
      addAllFeedbacks State{..}
    else return ()
eventCallback State{..} control = do
  runMaybeT_ $ (respondToControl State{..} control >>= performOutput State{..})
  hMoved control
  midiFeedback State{..} control
  return ()

main :: IO ()
main = do
  state <- stateFromConf . head =<< getArgs

  runGUI state

  addAllFeedbacks state

  setLeds state (MidiButtonValue True) (allLeds state)
  threadDelay 500000
  setLeds state (MidiButtonValue False) (allLeds state)

  _ <- register (eBankSwitch state) $ \_ -> refreshFeedbacks state ((>> return ()) . flip (setLed state) (MidiButtonValue False))
  hBankSwitch state ()

  updateBankLeds state

  processEvents state (eventCallback state)

