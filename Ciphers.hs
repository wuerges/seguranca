import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as U
import qualified Modular as M
import Data.Word

ceasar1 :: Int -> Word8 -> Word8
ceasar1 k w = fromIntegral $ M.msum 256 k (fromIntegral w)

c_ceasar :: Int -> B.ByteString -> B.ByteString
c_ceasar k t = B.map (ceasar1 k) t

d_ceasar :: Int -> B.ByteString -> B.ByteString
d_ceasar k t = B.map (ceasar1 (k * (-1))) t


c_transpose :: Int -> B.ByteString -> B.ByteString
c_transpose k bs = B.concat $ B.transpose $ split_equals k bs

d_transpose k bs = c_transpose k' bs
    where k' = ceiling $ fromIntegral (B.length bs) / fromIntegral k


split_equals :: Int -> B.ByteString -> [B.ByteString]
split_equals n bs = 
    if B.length bs <= n then [B.take n (bs ++ bs)]
                        else [B.take n bs] ++ split_equals n (B.drop n bs)


test_cipher c d k cleartext = d k ciphertext == cleartext
    where ciphertext = c k cleartext
    

