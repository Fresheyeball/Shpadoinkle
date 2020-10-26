{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}


module Shpadoinkle.Isreal.Types where


import           Data.Aeson           (FromJSON, ToJSON)
import           Data.ByteString.Lazy (ByteString)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Servant.API


data Options = Options
  { territory :: FilePath
  , swan      :: FilePath
  } deriving (Eq, Ord, Show, Generic)


newtype CompileError = CompileError Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)


newtype Code = Code ByteString deriving (Eq, Ord, Show, Generic)
deriving instance MimeUnrender OctetStream Code
deriving instance MimeRender   OctetStream Code


newtype SnowToken = SnowToken Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)


type API = "echo" :> Capture "echo" Text :> Get '[PlainText] Text
   :<|> "compile"
     :> Capture "token" SnowToken
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
