module Modular where

import Data.Word

msum :: Int -> Int -> Int -> Int
msum m a b = (a + b) `mod` m


