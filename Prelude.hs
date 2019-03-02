{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, FunctionalDependencies #-}

module Prelude
  ( upTo
  , mkArray
  , padLeft
  , bytes
  , unbytes
  , invert
  , lookupKeysWithValue
  , Factory (Value, Function)
  , runFactory
  , runMaybeT_
  , sequence_
  , mapM_
  , head
  , tail
  , lookup
  , (!)
  , fromList
  , toList
  , module BasePrelude
  ) where

import BasePrelude hiding (lookup, sequence_, mapM_, head, tail)
import qualified BasePrelude
import Control.Arrow (first)
import Control.Monad (void)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Array (Array, Ix, listArray)
import qualified Data.Array as Array ((!))
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap (lookup, fromList, toList)
import Data.Bool (bool)
import Data.List (unfoldr)
import Data.Map.Strict (Map, keys)
import qualified Data.Map.Strict as Map (filter, lookup, (!), fromList, toList)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, toList)
import Data.Tuple (swap)
import Data.Word (Word8)

upTo :: Int -> [Int]
upTo n = [0..n-1]

mkArray :: [a] -> Array Int a
mkArray xs = listArray (0, length xs - 1) xs

padLeft :: Int -> a -> [a] -> [a]
padLeft n x xs = replicate (n - length xs) x ++ xs

bytes :: Integral a => a -> [Word8]
bytes = unfoldr f
  where f 0 = Nothing
        f n = Just . first fromIntegral . swap . divMod n $ 256

unbytes :: Integral a => [Word8] -> a
unbytes = foldr (\x y -> fromIntegral x + y * 256) 0

invert :: Bool -> Float -> Float
invert = bool id (1.0 -)

lookupKeysWithValue :: (Ord k, Eq a) => a -> Map k a -> [k]
lookupKeysWithValue v = keys . Map.filter (== v)

data Factory a b = Value b | Function (a -> b)

runFactory :: Factory a b -> a -> b
runFactory (Value v)    = const v
runFactory (Function f) = f

runMaybeT_ :: Monad m => MaybeT m () -> m ()
runMaybeT_ = void . runMaybeT

sequence_ :: (Foldable t, Monad m) => t (m ()) -> m ()
sequence_ = BasePrelude.sequence_

mapM_ :: (Foldable t, Monad m) => (a -> m ()) -> t a -> m ()
mapM_ = BasePrelude.mapM_

head :: [a] -> Maybe a
head  []   = Nothing
head (x:_) = Just x

tail :: [a] -> Maybe [a]
tail  []    = Nothing
tail (_:xs) = Just xs

class Lookupable t k a | t -> k, t -> a where
  lookup :: k -> t -> Maybe a

instance Eq a => Lookupable [(a, b)] a b where
  lookup = BasePrelude.lookup

instance Ord k => Lookupable (Map k a) k a where
  lookup = Map.lookup

instance (Ord k, Ord a) => Lookupable (Bimap k a) k a where
  lookup = Bimap.lookup

class Indexable t i a | t -> i, t -> a where
  (!) :: t -> i -> a

instance Ix i => Indexable (Array i e) i e where
  (!) = (Array.!)

instance Ord k => Indexable (Map k a) k a where
  (!) = (Map.!)

class Listable t a | t -> a where
  fromList :: [a] -> t
  toList :: t -> [a]

instance Ord a => Listable (Set a) a where
  fromList = Set.fromList
  toList = Set.toList

instance Ord k => Listable (Map k a) (k, a) where
  fromList = Map.fromList
  toList = Map.toList

instance (Ord k, Ord a) => Listable (Bimap k a) (k, a) where
  fromList = Bimap.fromList
  toList = Bimap.toList

