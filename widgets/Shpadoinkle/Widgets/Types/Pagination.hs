{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}


module Shpadoinkle.Widgets.Types.Pagination
  ( CurrentScrollY (..)
  , Offset (..)
  , Length (..)
  , Page (..)
  ) where


import           Data.Aeson
import qualified Data.Attoparsec.Text        as A
import           Data.Either.Combinators     (mapLeft)
import           Data.Text                   (pack)
import           GHC.Generics
import           Language.Javascript.JSaddle hiding (JSM, MonadJSM)
import           Servant.API                 (FromHttpApiData (..),
                                              ToHttpApiData (..))
import           Shpadoinkle


newtype CurrentScrollY = CurrentScrollY Int -- measured in pixels
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Read, Show, Num, Enum, Real, Integral, NFData)


newtype Length = Length Int
  deriving (Eq, Ord, Generic, Read, Show, Num, Enum, Real, Integral, ToHttpApiData, FromHttpApiData)

instance ToJSON    Length
instance FromJSON  Length
instance ToJSVal   Length
instance FromJSVal Length
instance NFData    Length


newtype Offset = Offset Int
  deriving (Eq, Ord, Generic, Read, Show, Num, Enum, Real, Integral, ToHttpApiData, FromHttpApiData)

instance ToJSON    Offset
instance FromJSON  Offset
instance ToJSVal   Offset
instance FromJSVal Offset
instance NFData    Offset


data Page = Page { pageOffset :: Offset, pageLength :: Length }
  deriving (Eq, Ord, Generic, Read, Show)

instance ToJSON    Page
instance FromJSON  Page
instance ToJSVal   Page
instance FromJSVal Page
instance NFData    Page

instance ToHttpApiData Page where
  toUrlPiece (Page off len) = toUrlPiece off <> "," <> toUrlPiece len
  toQueryParam pg = toUrlPiece pg

instance FromHttpApiData Page where
  parseUrlPiece = (mapLeft pack .) . A.parseOnly $ do
    off <- Offset <$> A.signed A.decimal
    _ <- A.char ','
    len <- Length <$> A.signed A.decimal
    return $ Page off len
