import qualified Ciphers as C
import qualified Data.ByteString as B
import qualified System.IO as H


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
       hClose hInput
       hClose hOutput
