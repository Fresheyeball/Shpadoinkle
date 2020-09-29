{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


{-|
  HTML is a Servant combinator for the "text/html" Mime type, equipped with an instances
  for serving Shpadoinkle's 'Html' type.
-}


module Shpadoinkle.Router.HTML (HTML) where


#ifndef ghcjs_HOST_OS


import qualified Data.ByteString.Lazy       as BSL
import qualified Data.List.NonEmpty         as NE
import           Data.Text.Encoding
import qualified Network.HTTP.Media         as M
import           Servant.API                (Accept (..), MimeRender (..))

import           Shpadoinkle
import           Shpadoinkle.Backend.Static (renderStatic)


data HTML


instance Accept HTML where
    contentTypes _ =
      "text" M.// "html" M./: ("charset", "utf-8") NE.:|
      ["text" M.// "html"]


instance MimeRender HTML (Html m a) where
  mimeRender _ =  BSL.fromStrict . encodeUtf8 . renderStatic


#else


data HTML


#endif
