module Ciphers where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as U
import qualified Modular as M
import qualified Data.Map as Map
import Data.Word

type Cipher a = a -> B.ByteString -> B.ByteString

az = B.pack [0..255]

ceasar1 :: Int -> Word8 -> Word8
ceasar1 k w = fromIntegral $ M.msum 256 k (fromIntegral w)

c_ceasar :: Cipher Int
c_ceasar k t = B.map (ceasar1 k) t

d_ceasar :: Cipher Int
d_ceasar k t = B.map (ceasar1 (k * (-1))) t


c_transpose :: Cipher Int
c_transpose k bs = B.concat $ B.transpose $ split_equals k bs

d_transpose :: Cipher Int
d_transpose k bs = c_transpose k' bs
    where k' = ceiling $ fromIntegral (B.length bs) / fromIntegral k



build_subsfunction :: B.ByteString -> B.ByteString -> Word8 -> Word8
build_subsfunction ks vs  = f
    where f c = Map.findWithDefault 0 c m
          m = Map.fromList (B.zip vs ks)

c_substitution :: Cipher B.ByteString
c_substitution k bs = B.map (build_subsfunction k az) bs

d_substitution :: Cipher B.ByteString
d_substitution k bs = B.map (build_subsfunction az k) bs



pad_multiple :: Int -> B.ByteString -> B.ByteString
pad_multiple n bs = B.concat [bs, spaces (n - (B.length bs `mod` n))]

spaces :: Int -> B.ByteString
spaces n  = U.pack (take n $ repeat ' ')

split_equals :: Int -> B.ByteString -> [B.ByteString]
split_equals n bs = 
    if B.length bs <= n then [bs]
                        else [B.take n bs] ++ split_equals n (B.drop n bs)


test_cipher c d k cleartext = d k ciphertext == cleartext
    where ciphertext = c k cleartext
    

bruteforce :: [a] -> (Cipher a) -> B.ByteString -> B.ByteString -> Maybe a
bruteforce [] decipher cleartext ciphertext = Nothing
bruteforce (k:ks) decipher cleartext ciphertext = 
    if decipher k ciphertext == cleartext then Just k
                                          else bruteforce ks decipher cleartext ciphertext
