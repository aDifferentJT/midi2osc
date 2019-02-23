{-# LANGUAGE TupleSections, RecordWildCards, MultiParamTypeClasses, InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Feedback
  ( addFeedback
  , addAllFeedbacks
  , clearAllFeedbacks
  , addChannelFeedbacks
  , addActionFeedbacks
  , clearChannelFeedbacks
  , clearActionFeedbacks
  ) where

import Core
import Midi
import OSC
import OutputCore
import Utils

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Bimap (keysR)
import Data.List ((\\))
import Data.Map.Strict (elems, (!))
import Data.Maybe (fromJust)

registerFeedback :: State -> Output -> (Float -> IO ()) -> IO ()
registerFeedback State{..} (OSC conn inverted path) f = registerCallback (oscConnections ! conn) path (f . invert inverted)
registerFeedback _          _                       _ = return ()

unregisterFeedback :: State -> Output -> IO ()
unregisterFeedback State{..} (OSC conn _ path) = unregisterCallback (oscConnections ! conn) path
unregisterFeedback _          _                = return ()

askForFeedback :: State -> Output -> IO ()
askForFeedback State{..} (OSC conn _ path) = askForOSCValue (oscConnections ! conn) path
askForFeedback _          _                = return ()

instance Feedback where
  addFeedback :: State -> Control -> IO ()
  addFeedback State{..} control = (>> return ()) . runMaybeT $ do
    output <- MaybeT $ outputForControl State{..} <$> currentMapping <*> return control
    lift $ registerFeedback State{..} output (midiFeedbackFromFloat State{..} control)

  clearFeedback :: State -> Control -> IO ()
  clearFeedback State{..} control = (>> return ()) . runMaybeT $ do
    output <- MaybeT $ outputForControl State{..} <$> currentMapping <*> return control
    lift $ unregisterFeedback State{..} output

  clearAllFeedbacks :: State -> IO ()
  clearAllFeedbacks State{..} = do
    sequence_ . map unregisterAllCallbacks . elems $ oscConnections

  addAllFeedbacks :: State -> IO ()
  addAllFeedbacks State{..} = do
    clearAllFeedbacks State{..}
    currentMapping
      >>= (\m -> sequence_ . map (\c -> registerFeedback State{..} (fromJust . outputForControl State{..} m $ c) . midiFeedbackFromFloat State{..} $ c) . controlsInMapping State{..} $ m)

  addChannelFeedbacks :: State -> Channel -> IO ()
  addChannelFeedbacks state = sequence_ . map (addFeedback state) . controlsForChannel state

  addActionFeedbacks :: State -> Action -> IO ()
  addActionFeedbacks state = sequence_ . map (addFeedback state) . controlsForAction state

  clearChannelFeedbacks :: State -> Channel -> IO ()
  clearChannelFeedbacks state = sequence_ . map (clearFeedback state) . controlsForChannel state

  clearActionFeedbacks :: State -> Action -> IO ()
  clearActionFeedbacks state = sequence_ . map (clearFeedback state) . controlsForAction state

  refreshFeedbacks :: State -> (Control -> IO ()) -> IO ()
  refreshFeedbacks State{..} f = do
    mapping <- currentMapping
    let allControls = keysR profile
    let activeControls = controlsInMapping State{..} mapping
    let inactiveControls = allControls \\ activeControls
    sequence_ . map f $ inactiveControls
    sequence_ . map (askForFeedback State{..} . fromJust . outputForControl State{..} mapping) $ activeControls

