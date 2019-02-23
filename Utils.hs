module Utils
  ( upTo
  , mkArray
  , bytes
  , unbytes
  , invert
  , lookupKeysWithValue
  , Factory (Value, Function)
  , runFactory
  , infLoop
  , runMaybeT_
  ) where

import Prelude hiding (filter)
import Control.Arrow (first)
import Control.Monad (void)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Array (Array, listArray)
import Data.Bool (bool)
import Data.List (unfoldr)
import Data.Map.Strict (Map, filter, keys)
import Data.Tuple (swap)
import Data.Word (Word8)

upTo :: Int -> [Int]
upTo n = [0..n-1]

mkArray :: [a] -> Array Int a
mkArray xs = listArray (0, length xs - 1) xs

bytes :: Integral a => a -> [Word8]
bytes = unfoldr f
  where f 0 = Nothing
        f n = Just . first fromIntegral . swap . divMod n $ 256

unbytes :: Integral a => [Word8] -> a
unbytes = foldr (\x y -> fromIntegral x + y * 256) 0

invert :: Bool -> Float -> Float
invert = bool id (1.0 -)

lookupKeysWithValue :: (Ord k, Eq a) => a -> Map k a -> [k]
lookupKeysWithValue v = keys . filter (== v)

data Factory a b = Value b | Function (a -> b)

runFactory :: Factory a b -> a -> b
runFactory (Value v)    = const v
runFactory (Function f) = f

infLoop :: Monad m => m a -> m a
infLoop x = x >> infLoop x

runMaybeT_ :: Monad m => MaybeT m () -> m ()
runMaybeT_ = void . runMaybeT
