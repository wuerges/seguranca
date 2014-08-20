import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as U
import qualified Modular as M
import Data.Word

ceasar1 :: Int -> Word8 -> Word8
ceasar1 k w = fromIntegral $ M.msum 256 k (fromIntegral w)

ceasar :: Int -> B.ByteString -> B.ByteString
ceasar k t = B.map (ceasar1 k) t
