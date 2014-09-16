import Dictionary
import Ciphers
import System.Environment

main = do
    (c:f1:f2:args) <- getArgs
    let dec = case c of 
                "transpose" -> d_transpose
                "ceasar" -> d_ceasar
    rs <- bruteforceLanguageFile f1 f2 dec [1..100] 
    putStrLn $ "resultado" ++ show rs

