{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map once" #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use isJust" #-}

module XLSXReader ( 
    xlsxReader,
    getClear ) where

import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import Control.Lens
import Data.Text hiding ( 
    map,
    filter,
    head,
    words,
    unwords,
    tail,
    dropWhile )
import Data.Map hiding ( 
    map,
    filter )
import Prelude hiding ( 
    lookup )
import Debug.Trace ( 
    trace )

{-
import Data.Encoding
import Data.Encoding.CP1251
import Data.Encoding.UTF8
-}

xlsxReader :: String -> IO (Map (Int, Int) Text)
xlsxReader path = do
    bs <- L.readFile path
    let file = toXlsx bs
    return $ getVals file

getVals :: Xlsx -> Map (Int, Int) Text
getVals (Xlsx ((_, dirty) : datas) _ _ _ _) = table
    where table = 
            fromList $
            map (\(x, Just tp) -> (x, trace (unpack $ cellToText tp) (cellToText tp))) $
            filter (\(x, y) -> y /= Nothing) $
            map (\(x, y) -> (x, _cellValue y)) $
            toList $ 
            _wsCells dirty

cellToText :: CellValue -> Text
cellToText (CellText text) = text
cellToText s = pack $ unwords $ tail $ words $ show s

-- no type
getClear path = do
    bs <- L.readFile path
    let file = toXlsx bs
    return $ map snd $ toList $_wsCells $ snd $ head $ _xlSheets file
