{-# LANGUAGE DeriveGeneric, DefaultSignatures, TupleSections #-}

module Main (main) where

import Core
import GUI
import Midi
import Output
import Utils

import Prelude hiding (readFile, writeFile, lookup)
import Control.Monad.Trans.Maybe
import qualified Data.Array as Array
import Data.IORef
import Data.Map.Strict (lookup, empty)
import qualified Data.Serialize as Serialize
import System.Environment

import Reactive.Threepenny         hiding (empty)

respondToControl :: State -> MidiControlState -> MaybeT IO (Output, Float)
respondToControl state control = do
  v <- justT $ midiValueFromState state control
  o <- MaybeT . (lookup (midiControlFromState control) <$>) $ currentMapping state
  return (o, v)

eventCallback :: State -> MidiControlState -> IO ()
eventCallback state control = do
  selectingB <- readIORef . selecting $ state
  runMaybeT $ if not selectingB then respondToControl state control >>= performOutput state else nothingT
  hMoved state control

main :: IO ()
main = do
  midiInitialise

  state <- defaultState =<< getArgs

  runGUI state

  device <- readLn :: IO Int
  stream <- either (error "Cannot open device") id <$> openInput device

  processEvents (eventCallback state) stream

