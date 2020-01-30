{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}


module Shpadoinkle.Router.Server where


import           Data.ByteString.Lazy           as BS
import           Data.Text.Encoding
import           GHC.TypeLits
import           Network.Wai
import           Network.Wai.Application.Static
import           Servant.API
import           Servant.Server
import           Servant.Server.StaticFiles
import           WaiAppStatic.Types

import           Shpadoinkle
import           Shpadoinkle.Backend.Static
import           Shpadoinkle.Router


toFile :: Piece -> ByteString -> File
toFile p bs = File
  { fileGetSize     = fromIntegral $ BS.length bs
  , fileToResponse  = \status headers -> responseLBS status headers bs
  , fileName        = p
  , fileGetHash     = pure Nothing
  , fileGetModified = Nothing
  }


defaultSPAServerSettings :: FilePath -> Html m a -> StaticSettings
defaultSPAServerSettings root html = settings { ssLookupFile = orIndex }
  where

  settings   = defaultWebAppSettings root

  orIndex ps = do
    let file ps' = toFile ps' . BS.fromStrict . encodeUtf8 . renderStatic
    res <- ssLookupFile settings ps
    return $ case (res, toPieces ["index.html"]) of
      (LRNotFound, Just [ps'])                           -> LRFile $ file ps' html
      (_,          Just [ps']) | [ps'] == ps || ps == [] -> LRFile $ file ps' html
      _                                                  -> res


ui :: FilePath -> Html n a -> ServerT Raw m
ui root = serveDirectoryWith . defaultSPAServerSettings root


class ServeRouter layout route where
  serveUI :: FilePath -> (route -> Html m a) -> layout :>> route -> Server layout

instance (ServeRouter x r, ServeRouter y r)
  => ServeRouter (x :<|> y) r where

  serveUI :: FilePath -> (r -> Html m a) -> (x :<|> y) :>> r -> Server (x :<|> y)
  serveUI root view (x :<|> y) = serveUI @x root view x :<|> serveUI @y root view y

instance ServeRouter sub r
  => ServeRouter (Capture sym x :> sub) r where

  serveUI :: FilePath -> (r -> Html m a) -> (x -> sub :>> r) -> Server (Capture sym x :> sub)
  serveUI root view = (serveUI @sub root view .)

instance ServeRouter sub r
  => ServeRouter (QueryParam sym x :> sub) r where

  serveUI :: FilePath -> (r -> Html m a) -> (Maybe x -> sub :>> r) -> Server (QueryParam sym x :> sub)
  serveUI root view = (serveUI @sub root view .)

instance ServeRouter sub r
  => ServeRouter (QueryParams sym x :> sub) r where

  serveUI :: FilePath -> (r -> Html m a) -> ([x] -> sub :>> r) -> Server (QueryParams sym x :> sub)
  serveUI root view = (serveUI @sub root view .)

instance ServeRouter sub r
  => ServeRouter (QueryFlag sym :> sub) r where

  serveUI :: FilePath -> (r -> Html m a) -> (Bool -> sub :>> r) -> Server (QueryFlag sym :> sub)
  serveUI root view = (serveUI @sub root view .)

instance ServeRouter sub r
  => ServeRouter ((path :: Symbol) :> sub) r where

  serveUI :: FilePath -> (r -> Html m a) -> (path :> sub) :>> r -> Server (path :> sub)
  serveUI root view = serveUI @sub root view

instance ServeRouter Raw r where
  serveUI :: FilePath -> (r -> Html m a) -> Raw :>> r -> Server Raw
  serveUI root view = ui root . view
