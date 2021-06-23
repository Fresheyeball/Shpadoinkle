{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Types where


import Control.DeepSeq (NFData)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.CountryCodes
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import Language.Javascript.JSaddle hiding (val)
import Servant.API
import Shpadoinkle.Html (text)
import Shpadoinkle.Widgets.Table
import Shpadoinkle.Widgets.Table.Lazy
import Shpadoinkle.Widgets.Types (Humanize (..))
import Test.QuickCheck

import StockName


instance ToJSVal CountryCode where
  toJSVal = toJSVal . toText

instance FromJSVal CountryCode where
  fromJSVal val = (fromMText =<<) <$> fromJSVal val


data Sex = Male | Female
  deriving (Eq, Ord, Show, Generic)

instance ToJSVal Sex
instance FromJSVal Sex
instance NFData Sex

instance ToJSON Sex where
  toJSON Male = toJSON @Text "male"
  toJSON Female = toJSON @Text "female"

instance FromJSON Sex where
  parseJSON (String "male") = return Male
  parseJSON (String "female") = return Female
  parseJSON _ = fail "not a valid sex"


data Person = Person
            { name   :: Text
            , age    :: Int
            , sex    :: Sex
            , origin :: CountryCode }
  deriving (Eq, Show, Generic)

instance ToJSVal Person
instance FromJSVal Person
instance NFData Person

instance ToJSON Person where
  toJSON p = object [ "name" .= name p
                    , "age" .= age p
                    , "sex" .= sex p
                    , "origin" .= origin p ]

instance FromJSON Person where
  parseJSON = withObject "Person" $ \p -> do
    n <- p .: "name"
    a <- p .: "age"
    s <- p .: "sex"
    o <- p .: "origin"
    return (Person n a s o)


instance Arbitrary Sex where
  arbitrary = oneof [return Male, return Female]


instance Arbitrary CountryCode where
  arbitrary = elements (fst <$> allNames)


instance Arbitrary Person where
  arbitrary = Person <$> (unStockName <$> arbitrary) <*> choose (0,120) <*> arbitrary <*> arbitrary


type Api = QueryParam' '[Required] "page" Page
        :> QueryParam' '[Required] "sort" (SortCol FilteredTable)
        :> QueryParam' '[Required] "filters" TableFilters
        :> Get '[JSON] [Person]


data TableFilters = TableFilters
                  { bySex    :: Maybe Sex
                  , byOrigin :: Set CountryCode }
  deriving (Eq, Show, Generic)

instance ToJSON   TableFilters
instance FromJSON TableFilters
instance NFData   TableFilters

instance ToHttpApiData TableFilters where
  toUrlPiece = decodeUtf8 . BSL.toStrict . encode
  toQueryParam = toUrlPiece

instance FromHttpApiData TableFilters where
  parseUrlPiece = maybe (Left "could not decode TableFilters JSON") Right
                . decode . BSL.fromStrict . encodeUtf8
  parseQueryParam = parseUrlPiece


data FilteredTable = FilteredTable
                   { contents :: [Person]
                   , filters  :: TableFilters }
  deriving (Eq, Show, Generic)

instance NFData FilteredTable

instance LazyTabular FilteredTable where
  countRows _ = 100000


data instance Row FilteredTable = PersonRow { unRow :: Person }


data instance Column FilteredTable = Name | Age | Sex | Origin
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance ToJSON   (Column FilteredTable)
instance FromJSON (Column FilteredTable)
instance NFData   (Column FilteredTable)

instance Humanize (Column FilteredTable) where
  humanize = humanize . show


instance Tabular FilteredTable where
  type Effect FilteredTable m = MonadJSM m

  toRows xs = PersonRow <$> contents xs

  toFilter (filters -> TableFilters {..}) (unRow -> p) = sexFilter && countryFilter
    where sexFilter = case bySex of
            Just s  -> sex p == s
            Nothing -> True
          countryFilter = Set.null byOrigin || Set.member (origin p) byOrigin

  sortTable (SortCol c s) (unRow -> a) (unRow -> b) =
    case c of
      Name   -> compareOn s (name a)   (name b)
      Age    -> compareOn s (age a)    (age b)
      Sex    -> compareOn s (sex a)    (sex b)
      Origin -> compareOn s (origin a) (origin b)

  toCell _ (unRow -> p) Name   = [text (name p)]
  toCell _ (unRow -> p) Age    = [text (pack (show (age p)))]
  toCell _ (unRow -> p) Sex    = [text (pack (show (sex p)))]
  toCell _ (unRow -> p) Origin = [text (toName (origin p))]


type Model = (FilteredTable, SortCol FilteredTable)

