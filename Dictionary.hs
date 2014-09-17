module Dictionary where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified System.IO as H
import qualified Data.Set as S
import Ciphers
import Fileio

type Dict = S.Set B.ByteString

dwords :: B.ByteString -> [B.ByteString]
dwords text = C.splitWith f text
    where f t = t `elem` [' ', '\n', '\r', '\t']
          

mkDictionary :: H.FilePath -> IO Dict
mkDictionary input =
    do hInput <- H.openFile input H.ReadMode 
       text <- B.hGetContents hInput
       H.hClose hInput
       return $ foldr i S.empty (dwords text)
            where i bs s = if B.length bs > 3 then S.insert bs s else s 

detectLanguage :: Dict -> B.ByteString -> Maybe B.ByteString
detectLanguage d bs = dt d (dwords bs)
    where dt d (w:ws) = if S.member w d then Just w else dt d ws
          dt d [] = Nothing

detectLanguageImprov :: Dict -> B.ByteString -> [B.ByteString]
detectLanguageImprov d bs = dt [] d (dwords bs)
    where dt fs d (w:ws) = if S.member w d then dt (w:fs) d ws else dt fs d ws
          dt fs d [] = fs

detectLanguageFile :: H.FilePath -> H.FilePath -> IO (Maybe B.ByteString)
detectLanguageFile dict test = do
    d <- mkDictionary dict
    bs <- getBSFile test
    return $ detectLanguage d bs

bruteforceLanguageFile :: H.FilePath -> H.FilePath -> Cipher a -> [a] -> IO [(a, [B.ByteString])]
bruteforceLanguageFile dict test decipher ks = do
    d <- mkDictionary dict
    bs <- getBSFile test
    let results = bruteforceF ks decipher bs (detectLanguageImprov d)
    return $ [(k, bss) | (k, bss) <- results, length bss > 3]

