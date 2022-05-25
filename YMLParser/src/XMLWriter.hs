{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
-- {-# LANGUAGE QuasiQuotes #-}

module XMLWriter ( 
    xmlWriter,
    offersToXML ) where

import Prelude hiding ( 
    writeFile, 
    foldr )
import Text.XML
import Data.Char
import Data.Text ( 
    Text,
    unpack,
    pack,
    foldr )
import Data.Map ( 
    Map (..),
    fromList,
    toList )
import ParseTypes
-- import Text.Hamlet.XML

-- writng data into xml
xmlWriter :: String -> Element -> IO ()
xmlWriter filename element =
    writeFile def filename $
        Document (Prologue [] Nothing []) element []

offersToXML :: Offers -> IO Element
offersToXML offers = return $
    Element "yml_catalog" []
        [
            NodeElement $ Element "categories" []
                [ NodeElement $ Element "category"
                    (rejustAttrs [ (Name "id" Nothing Nothing, Just $ catId cat), (Name "parentId" Nothing Nothing, parentId cat) ])
                    [ NodeContent $ catName cat ] | cat <- noRepeat $ map catOffer offers ],
            NodeElement $ Element "offers" []
                [ NodeElement $ Element "offer" 
                    (doId $ articleOrSkuCode offer)
                    ([
                        NodeElement $ Element "sku_code"
                            []
                            [ NodeContent $ articleOrSkuCode offer ],
                        NodeElement $ Element "price"
                            []
                            [ NodeContent $ pack $ show $ price offer ],
                        NodeElement $ Element "categoryId"
                            []
                            [ NodeContent $ catId $ catOffer offer ],
                        NodeElement $ Element "name"
                            []
                            [ NodeContent $ offerName offer ],
                        NodeElement $ Element "description"
                            []
                            [ NodeContent $ description offer ],
                        NodeElement $ Element "quantity"
                            []
                            [ NodeContent $ pack $ show $ quantity offer ],
                        NodeElement $ Element "barcode"
                            []
                            [ NodeContent $ pack $ show $ barcode offer ]
                    ] ++ [ 
                        NodeElement $ Element "picture"
                        []
                        [ NodeContent pic ] | pic <- pics offer
                    ] ++ [
                        NodeElement $ Element "param"
                        (rejustAttrs [ (Name "name" Nothing Nothing, Just $ name par), (Name "unit" Nothing Nothing, unit par) ])
                        [ NodeContent $ param par ] | par <- params offer
                    ]) | offer <- offers ]
        ]
    where
        noRepeat :: Eq a => [a] -> [a]
        noRepeat [] = []
        noRepeat (x : xs) = x : (noRepeat . filter (/= x)) xs
        rejustAttrs :: Map Name (Maybe Text) -> Map Name Text
        rejustAttrs = fromList .
                      map (\(x, Just y) -> (x, y)) .
                      filter (\(x, y) -> case y of
                                          Nothing -> False
                                          y -> True) .
                      toList
        doId :: Text -> Map Name Text
        doId txt = fromList 
                    [(Name "id" Nothing Nothing, txt) 
                    | foldr (\chr b -> b && isDigit chr) True txt]

{-xmlWriter offers fileName =
    writeFile def fileName $
        Document (Prologue [] Nothing []) root []
    where
        root = Element "html" [xml |
            <head>
                <title>
                    Hi
            <body>
                <p>
                    foo bar baz
        |]-}
