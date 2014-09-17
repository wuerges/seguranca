import Fileio
import qualified Ciphers as C
import System.Environment

main = do [cipher, mode, key, input, output] <- getArgs
          case (cipher, mode) of 
            ("ceasar", "code") -> encode C.c_ceasar (read key) input output
            ("ceasar", "decode") -> encode C.d_ceasar (read key) input output
            ("transposition", "code") -> encode C.c_transpose (read key) input output
            ("transposition", "decode") -> encode C.d_transpose (read key) input output

