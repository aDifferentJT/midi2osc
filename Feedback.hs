{-# LANGUAGE TupleSections, RecordWildCards, MultiParamTypeClasses, InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Feedback
  ( addFeedback
  , clearFeedback
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

import Prelude hiding (sequence)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import Data.Map.Strict (Map, empty, findWithDefault, insert, delete, elems, fromList, (!))
import Data.Maybe (fromJust)
import Data.Traversable (sequence)
import System.IO.Unsafe (unsafePerformIO)

unRegFeedbacks :: IORef (Map Control (IO ()))
unRegFeedbacks = unsafePerformIO (newIORef empty)

registerFeedback :: State -> Output -> (Float -> IO ()) -> IO (IO ())
registerFeedback state (OSC c inverted path) f = forkIO (getOSCFeedback state f c inverted path) >>= return . killThread
registerFeedback _      _                    _ = return (return ())

getOSCFeedback :: State -> (Float -> IO ()) -> Connection -> Bool -> String -> IO ()
getOSCFeedback State{..} f c inverted path = infLoop $ do
  f . invert inverted =<< getOSCValueSync (oscConnections ! c) path
  threadDelay 500000

instance Feedback where
  addFeedback :: State -> Control -> IO ()
  addFeedback State{..} control = (>> return ()) . runMaybeT $ do
    unRegOld <- lift $ findWithDefault (return ()) control <$> readIORef unRegFeedbacks
    lift unRegOld
    output <- MaybeT $ outputForControl State{..} <$> currentMapping <*> return control
    unRegNew <- lift $ registerFeedback State{..} output (midiFeedbackFromFloat State{..} control)
    lift $ modifyIORef' unRegFeedbacks (insert control unRegNew)

  clearFeedback :: Control -> IO ()
  clearFeedback control = do
    unRegOld <- findWithDefault (return ()) control <$> readIORef unRegFeedbacks
    unRegOld
    modifyIORef' unRegFeedbacks (delete control)

  clearAllFeedbacks :: IO ()
  clearAllFeedbacks = do
    _ <- sequence . elems =<< readIORef unRegFeedbacks
    writeIORef unRegFeedbacks empty

  addAllFeedbacks :: State -> IO ()
  addAllFeedbacks State{..} = do
    clearAllFeedbacks
    currentMapping
      >>= (\m -> sequence . map (\c -> ((c,) <$>) . registerFeedback State{..} (fromJust . outputForControl State{..} m $ c) . midiFeedbackFromFloat State{..} $ c) . controlsInMapping State{..} $ m)
      >>= writeIORef unRegFeedbacks . fromList

  addChannelFeedbacks :: State -> Channel -> IO ()
  addChannelFeedbacks state = sequence_ . map (addFeedback state) . controlsForChannel state

  addActionFeedbacks :: State -> Action -> IO ()
  addActionFeedbacks state = sequence_ . map (addFeedback state) . controlsForAction state

  clearChannelFeedbacks :: State -> Channel -> IO ()
  clearChannelFeedbacks state = sequence_ . map clearFeedback . controlsForChannel state

  clearActionFeedbacks :: State -> Action -> IO ()
  clearActionFeedbacks state = sequence_ . map clearFeedback . controlsForAction state

