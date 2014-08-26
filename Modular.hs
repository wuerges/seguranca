module Modular where

import Data.Word

msum :: Int -> Int -> Int -> Int
msum m a b = (a + b) `mod` m



msumInt :: Integral a => Integral b => Int -> a -> b -> b
msumInt m a b = fromIntegral $ msum m (fromIntegral a) (fromIntegral b)



