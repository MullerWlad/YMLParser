{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Replace case with maybe" #-}
module Features ( 
    mapToOffers ) where

import Data.Map ( 
    Map (..),
    toList )
import Data.Map hiding ( 
    map )
import Data.Text ( 
    Text (..),
    pack,
    words )
import ParseTypes
import Prelude hiding ( 
    lookup,
    words )

findSquare :: Map (Int, Int) Text -> (Int, Int)
findSquare mp = (maximum $ map fst keysOnly, maximum $ map snd keysOnly)
    where keysOnly = map fst $ toList mp

mapToOffers :: Map (Int, Int) Text -> IO Offers
mapToOffers mp = return $ helper (3, 1) (findSquare mp)
    where helper (x, y) (xs, ys)
            | x > xs = []
            | otherwise =
                (Offer (rejust $ lookup (x, 1) mp)
                       (rejust $ lookup (x, 27) mp)
                       (OfferCat (pack "1") Nothing (pack "Велозапчасти, аксессуары"))
                       (words $ rejust $ lookup (x, 25) mp)
                       (rejust $ lookup (x, 2) mp)
                       (rejust $ lookup (x, 2) mp)
                       (rejust $ lookup (x, 26) mp)
                       [
                           Param (pack "Торговая марка: ") Nothing (rejust $ lookup (x, 14) mp),
                           Param (pack "Код товара: ") Nothing (rejust $ lookup (x, 16) mp)
                       ]
                       (rejust $ lookup (x, 24) mp)) : helper (x + 1, y) (xs, ys)
          rejust val = case val of
                        Nothing -> pack "nothing"
                        Just x -> x
