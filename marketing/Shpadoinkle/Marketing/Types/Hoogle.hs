{-# LANGUAGE OverloadedStrings #-}


module Shpadoinkle.Marketing.Types.Hoogle where


import           Data.Aeson       (FromJSON (parseJSON), ToJSON (toJSON),
                                   object, withObject, (.:))
import           Data.Aeson.Types (emptyObject)
import qualified Data.Text        as T


type URL = String


-- | A location of documentation.
data Target = Target
    {targetURL     :: URL -- ^ URL where this thing is located
    ,targetPackage :: Maybe (String, URL) -- ^ Name and URL of the package it is in (Nothing if it is a package)
    ,targetModule  :: Maybe (String, URL) -- ^ Name and URL of the module it is in (Nothing if it is a package or module)
    ,targetType    :: String -- ^ One of package, module or empty string
    ,targetItem    :: String -- ^ HTML span of the item, using @\<s0\>@ for the name and @\<s1\>@ onwards for arguments
    ,targetDocs    :: String -- ^ HTML documentation to show, a sequence of block level elements
    } deriving (Show,Eq,Ord)


instance ToJSON Target where
    toJSON (Target a b c d e f) = object [
      ("url" :: T.Text, toJSON a),
      ("package" :: T.Text, maybeNamedURL b),
      ("module" :: T.Text, maybeNamedURL c),
      ("type" :: T.Text, toJSON d),
      ("item" :: T.Text, toJSON e),
      ("docs" :: T.Text, toJSON f)
      ]
      where
        maybeNamedURL m = maybe emptyObject namedURL m
        namedURL (name, url) = object [("name" :: T.Text, toJSON name), ("url" :: T.Text, toJSON url)]


instance FromJSON Target where
  parseJSON = withObject "Target" $ \o ->
    Target <$> o .: ("url" :: T.Text)
           <*> o `namedUrl` ("package" :: T.Text)
           <*> o `namedUrl` ("module" :: T.Text)
           <*> o .: ("type" :: T.Text)
           <*> o .: ("item" :: T.Text)
           <*> o .: ("docs" :: T.Text)
    where namedUrl o' n = do
             mObj <- o' .: n
             if null mObj then pure Nothing
                        else do
                           pkName <- mObj .: ("name" :: T.Text)
                           pkUrl  <- mObj .: ("url" :: T.Text)
                           pure $ Just (pkName, pkUrl)
