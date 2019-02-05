module Core (Mapping, ButtonStates, State, filenames, eMoved, hMoved, eBankSwitch, hBankSwitch, selecting, mappings, currentMappingIndex, buttonStates, defaultState, currentMapping, save, open) where

import MidiCore
import OutputCore
import Utils

import Prelude hiding (readFile, writeFile)
import Control.Exception (throwIO, catch, IOException)
import Data.Array
import Data.ByteString (readFile, writeFile)
import Data.IORef
import Data.Map.Strict (Map, empty, fromList, toList)
import Data.Serialize
import Data.Word

import Reactive.Threepenny hiding (empty)

type Mapping = Map MidiControl Output
type ButtonStates = Map Word8 Bool

data State = State
  { filenames :: [String]
  , eMoved :: Event MidiControlState
  , hMoved :: Handler MidiControlState
  , eBankSwitch :: Event ()
  , hBankSwitch :: Handler ()
  , selecting :: IORef Bool
  , mappings :: IORef (Array Int Mapping)
  , currentMappingIndex :: IORef Int
  , buttonStates :: IORef ButtonStates
  }

mkState = (uncurry .) . uncurry . State

defaultState :: [String] -> IO State
defaultState fns = do
  ms <- mkArray <$> (sequence . map open $ fns)
  let btns = fromList
           . map (\(MidiButton n,_) -> (n, False))
           . filter (\c -> case c of (MidiButton _,_) -> True; _ -> False)
           . concat
           . map toList
           . elems
           $ ms
  mkState fns
          <$> newEvent
          <*> newEvent
          <*> newIORef False
          <*> newIORef ms
          <*> newIORef 0
          <*> newIORef btns

currentMapping :: State -> IO Mapping
currentMapping state = (!) <$> readIORef (mappings state) <*> readIORef (currentMappingIndex state)

save :: String -> Mapping -> IO ()
save filename mapping = writeFile filename . encode $ mapping

open :: String -> IO Mapping
open filename = catch (either (const . throwIO . userError $ "Invalid file") return . decode =<< readFile filename) (const . return $ empty :: IOException -> IO Mapping)

