import qualified Ciphers as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as U
import qualified System.IO as H
import System.Random
import System.Environment

randomize :: B.ByteString -> IO B.ByteString
randomize bs = do rs <- mapM (\x -> randomIO) [0..(B.length bs)]
                  return $ B.pack $ randomize_ rs (B.unpack bs)

randomize_ :: [Int] -> [a] -> [a]
randomize_ _ [] = []
randomize_ (n:ns) xs = l : randomize_ ns (fs ++ ls)
    where n' = n `mod` length xs
          (fs, l:ls) = splitAt n' xs 



hEncode :: C.Cipher a -> a -> H.Handle -> H.Handle -> IO ()
hEncode c k input output  = 
    do text <- B.hGetContents input
       let ciphertext = c k text
       B.hPut output ciphertext

encode :: C.Cipher a -> a -> H.FilePath -> H.FilePath -> IO ()
encode c k input output = 
    do hInput <- H.openFile input H.ReadMode 
       hOutput <- H.openFile output H.WriteMode 
       hEncode c k hInput hOutput
       H.hClose hInput
       H.hClose hOutput

hEncodeL :: C.Cipher a -> a -> H.Handle -> H.Handle -> IO ()
hEncodeL c k input output  = 
    do text <- B.hGetContents input
       let ls = U.lines text
       let ciphertext_ls = map (c k) ls
       B.hPut output (U.unlines ciphertext_ls)

encodeL :: C.Cipher a -> a -> H.FilePath -> H.FilePath -> IO ()
encodeL c k input output = 
    do hInput <- H.openFile input H.ReadMode 
       hOutput <- H.openFile output H.WriteMode 
       hEncodeL c k hInput hOutput
       H.hClose hInput
       H.hClose hOutput


main = do [cipher, mode, key, input, output] <- getArgs
          case (cipher, mode) of 
            ("ceasar", "code") -> encode C.c_ceasar (read key) input output
            ("ceasar", "decode") -> encode C.d_ceasar (read key) input output
            ("transposition", "code") -> encode C.c_transpose (read key) input output
            ("transposition", "decode") -> encode C.d_transpose (read key) input output
