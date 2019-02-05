module Utils (upTo, mapFst, mapSnd, mkArray, bytes, justT, nothingT) where

import Control.Monad.Trans.Maybe
import Data.Array
import Data.List
import Data.Tuple
import Data.Word

upTo :: Int -> [Int]
upTo n = [0..n-1]

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x, y) = (x, f y)

mkArray :: [a] -> Array Int a
mkArray xs = listArray (0, length xs - 1) xs

bytes :: Integral a => a -> [Word8]
bytes = unfoldr f
  where f 0 = Nothing
        f n = Just . mapFst fromIntegral . swap . divMod n $ 256

justT :: Monad m => m a -> MaybeT m a
justT = MaybeT . (Just <$>)

nothingT :: Monad m => MaybeT m a
nothingT = MaybeT . return $ Nothing

