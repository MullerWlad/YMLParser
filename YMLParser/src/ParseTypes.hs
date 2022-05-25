module ParseTypes ( 
    Offers (..),
    Param (..),
    OfferCat (..),
    Offer (..) ) where

import Data.Text

type Offers = [Offer]

data Param = Param {
    name :: Text,
    unit :: Maybe Text,
    param :: Text
} deriving (Show, Eq)

data OfferCat = OfferCat {
    catId :: Text,
    parentId :: Maybe Text,
    catName :: Text
} deriving (Show, Eq)

data Offer = Offer {
    articleOrSkuCode :: Text,
    price :: Text,
    catOffer :: OfferCat,
    pics :: [Text],
    offerName :: Text,
    description :: Text,
    quantity :: Text,
    params :: [Param],
    barcode :: Text
} deriving (Show, Eq)