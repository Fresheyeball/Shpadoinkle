{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


{-|
  HTML is a Servant combinator for the "text/html" MIME type, equipped with instances
  for serving Shpadoinkle's 'Html' type.
-}


module Shpadoinkle.Router.HTML where


import           Data.Kind                  (Type)


#ifndef ghcjs_HOST_OS


import qualified Data.ByteString.Lazy       as BSL
import qualified Data.List.NonEmpty         as NE
import           Data.Text.Encoding         (encodeUtf8)
import qualified Network.HTTP.Media         as M
import           Servant                    (Application, HasServer (..),
                                             Proxy (..), Raw, Tagged)
import           Servant.API                (Accept (contentTypes),
                                             HasLink (..), MimeRender (..))

import           Shpadoinkle                (Html)
import           Shpadoinkle.Backend.Static (renderStatic)


#else


import           Servant.API                (HasLink (..))


#endif


#ifndef ghcjs_HOST_OS


instance Accept HTML where
    contentTypes _ =
      "text" M.// "html" M./: ("charset", "utf-8") NE.:|
      ["text" M.// "html"]


instance MimeRender HTML (Html m a) where
  mimeRender _ =  BSL.fromStrict . encodeUtf8 . renderStatic


instance HasServer (Spa m a) context where
  type ServerT (Spa m a) m' = Tagged m' Application
  route _                   = route                  (Proxy @Raw)
  hoistServerWithContext _  = hoistServerWithContext (Proxy @Raw)


#endif


data HTML :: Type
data Spa :: (Type -> Type) -> Type -> Type


instance HasLink (Spa m a) where
  type MkLink (Spa m a) b = b
  toLink toA _ = toA
