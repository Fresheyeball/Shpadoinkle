{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}


module Shpadoinkle.Isreal.Types
  ( Options (..)
  , CompileError (..)
  , Code (..)
  , SnowToken, unSnowToken
  , genSnowToken, API, serve
  , SnowNonce(..)
  ) where


import           Control.DeepSeq         (NFData)
import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          Value (String))
import           Data.ByteString.Lazy    (ByteString)
import           Data.Proxy
import           Data.Text               (Text, pack)
import           Data.Text.Lazy          (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Time               (getCurrentTime)
import           GHC.Generics            (Generic)
import           Servant.API
import           System.Random           (Random (randomRIO))


data Options = Options
  { territory :: FilePath
  , swan      :: FilePath
  } deriving (Eq, Ord, Show, Generic)


newtype CompileError = CompileError { unCompileError :: Text }
  deriving stock Generic
  deriving newtype (Eq, Ord, Show, Read, ToJSON, FromJSON, NFData)


newtype Code = Code { unCode :: ByteString }
  deriving stock Generic
  deriving newtype (Eq, Ord, Show, Read, NFData)
deriving instance MimeUnrender OctetStream Code
deriving instance MimeRender   OctetStream Code


instance ToJSON Code where
  toJSON = String . toStrict . decodeUtf8 . unCode


instance FromJSON Code where
  parseJSON (String s) = pure . Code . encodeUtf8 $ fromStrict s
  parseJSON _          = fail "not a string, so not Code"


newtype SnowToken = SnowToken { unSnowToken :: Text }
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, ToHttpApiData, NFData)


serve :: SnowToken -> Link
serve = safeLink (Proxy @API) (Proxy @("serve"
  :> Capture "token" SnowToken
  :> Raw))


newtype SnowNonce = SnowNonce { unSnowNonce :: Int }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Read, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData, NFData, Num)


genSnowToken :: IO SnowToken
genSnowToken = do
  (cur, rand) <- (,) <$> getCurrentTime <*> randomRIO (1, 1000000 :: Double)
  return . SnowToken $ pack (show cur) <> "-" <> pack (show rand)


type API = "echo" :> Capture "echo" Text :> Get '[PlainText] Text
   :<|> "compile"
     :> Capture "token" SnowToken
     :> QueryParam' '[Required] "nonce" SnowNonce
     :> ReqBody '[OctetStream] Code
     :> Post    '[JSON] (Either CompileError Text)
   :<|> "clean"
     :> Capture "token" SnowToken
     :> Delete '[JSON, PlainText] Text
   :<|> "clean-all"
     :> Delete '[JSON, PlainText] Text
   :<|> "serve"
     :> Capture "token" SnowToken
     :> Raw
   :<|> Get '[PlainText] Text
