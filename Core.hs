module Core (Mapping, State, filenames, eMoved, hMoved, eBankSwitch, hBankSwitch, selecting, mappings, currentMappingIndex, defaultState, currentMapping, save, open) where

import MidiCore
import OutputCore
import Utils

import Prelude hiding (readFile, writeFile)
import Control.Exception (throwIO, catch, IOException)
import Data.Array (Array, (!))
import Data.ByteString (readFile, writeFile)
import Data.IORef (IORef, readIORef, newIORef)
import Data.Map.Strict (Map, empty)
import Data.Serialize (encode, decode)

import Reactive.Threepenny hiding (empty)

type Mapping = Map MidiControl Output

data State = State
  { filenames :: [String]
  , eMoved :: Event MidiControlState
  , hMoved :: Handler MidiControlState
  , eBankSwitch :: Event ()
  , hBankSwitch :: Handler ()
  , selecting :: IORef Bool
  , mappings :: IORef (Array Int Mapping)
  , currentMappingIndex :: IORef Int
  }

mkState = (uncurry .) . uncurry . State

defaultState :: [String] -> IO State
defaultState fns = do
  mkState fns
          <$> newEvent
          <*> newEvent
          <*> newIORef False
          <*> (newIORef . mkArray =<< (sequence . map open $ fns))
          <*> newIORef 0

currentMapping :: State -> IO Mapping
currentMapping state = (!) <$> readIORef (mappings state) <*> readIORef (currentMappingIndex state)

save :: String -> Mapping -> IO ()
save filename mapping = writeFile filename . encode $ mapping

open :: String -> IO Mapping
open filename = catch (either (const . throwIO . userError $ "Invalid file") return . decode =<< readFile filename) (const . return $ empty :: IOException -> IO Mapping)

