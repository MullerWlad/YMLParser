module Main where

import XMLWriter ( 
    xmlWriter,
    offersToXML )
import XLSXReader ( 
    xlsxReader )
import Features ( 
    mapToOffers )
import GHC.IO.Encoding (
    setLocaleEncoding,
    utf8 )

{-
import System.IO hiding (
    hPutStr, 
    hPutStrLn, 
    hGetLine, 
    hGetContents, 
    putStrLn )
import System.IO.UTF8
import Codec.Binary.UTF8.String (utf8Encode)
-}

parseExecutor :: String -> IO ()
parseExecutor filename =
    --setLocaleEncoding utf8 >>
    xlsxReader filename >>=
    mapToOffers >>=
    offersToXML >>=
    xmlWriter (takeWhile (/= '.') filename ++ ".xml")

main :: IO ()
main = 
    print "Choose file with directory" >>
    print "Example: C:/example/example.xlsx" >>
    putStrLn "file: " >>
    --setLocaleEncoding utf8 >>
    getLine >>= 
    parseExecutor >>
    putStrLn "Well done! Press enter" >>
    getLine >>= \x -> 
    return ()
