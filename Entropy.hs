--module Entropy where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Word
import Dictionary
import Fileio
import System.Environment

type FrequencyMap = M.Map Word8 Float

insertFM :: Word8 -> FrequencyMap -> FrequencyMap
insertFM w m = M.insertWith (+) w 1.0 m


frequencyMap :: B.ByteString -> FrequencyMap
frequencyMap bs = M.map (\x -> x / (fromIntegral $ B.length bs ::Float)) initial
    where initial = B.foldr insertFM M.empty bs

type PatternMap = M.Map [Int] (S.Set B.ByteString)

pattern :: B.ByteString -> [Int]
pattern bs = 
    let (_, _, acc) = B.foldl pattern_char (M.empty, 0, []) bs
    in reverse acc

pattern_char (m, i, acc) c = 
    case M.lookup c m of
        Just pos_c -> (m, i, pos_c:acc)
        Nothing -> (M.insert c i m, i+1, i:acc)

patternMap :: [B.ByteString] -> PatternMap
patternMap ws = M.fromListWith (S.union) (map (\w -> (pattern w, (S.singleton w))) ws)


type Match = M.Map Word8 [Word8]
type Prematch = ([B.ByteString], [B.ByteString])

matchWord :: (B.ByteString, B.ByteString) -> Match
matchWord (cw, tw) = M.fromListWith (++) [(c, [t]) | (c,t) <- B.zip cw tw]

match1Prematch :: Prematch -> [Match]
match1Prematch (cws, tws) = map matchWord [(cw, tw) | cw <- cws, tw <- tws]

combineMatches :: [Match] -> Match
combineMatches ms = M.unionsWith (++) ms

matchPatternMaps :: PatternMap -> PatternMap -> Match
matchPatternMaps ciphermap clearmap = 
    let keys = S.toList $ S.intersection (M.keysSet ciphermap) (M.keysSet clearmap)
        buildPrematch k = ( S.toList $ fromMaybe S.empty $ M.lookup k ciphermap
                          , S.toList $ fromMaybe S.empty $ M.lookup k clearmap)
        prematches = map buildPrematch keys
        matches = concat $ map match1Prematch prematches
    in combineMatches matches


main = do
    a : args <- getArgs
    bs <- getBSFile "trabalho4/cleartext/osda.txt"
    let ws = dwords bs
    let pm = patternMap $ take (read a) ws

    putStrLn $ show $ matchPatternMaps pm pm

