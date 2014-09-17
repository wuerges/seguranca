module Entropy where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Word
import Dictionary

type FrequencyMap = M.Map Word8 Float

insertFM :: Word8 -> FrequencyMap -> FrequencyMap
insertFM w m = M.insertWith (+) w 1.0 m


frequencyMap :: B.ByteString -> FrequencyMap
frequencyMap bs = M.map (\x -> x / (fromIntegral $ B.length bs ::Float)) initial
    where initial = B.foldr insertFM M.empty bs

type PatternMap = M.Map B.ByteString B.ByteString

pattern :: B.ByteString -> [Int]
pattern bs = 
    let (_, _, acc) = B.foldl pattern_char (M.empty, 0, []) bs
    in reverse acc

pattern_char (m, i, acc) c = 
    case M.lookup c m of
        Just pos_c -> (m, i, pos_c:acc)
        Nothing -> (M.insert c i m, i+1, i:acc)

