{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}


module Main where


import           Data.ByteString.Lazy           as BS
import           Data.Proxy
import           Data.Text.Encoding
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Options.Applicative
import           Servant.API
import           Servant.Server
import           Servant.Server.StaticFiles
import           WaiAppStatic.Types

import           Shpadoinkle
import           Shpadoinkle.Backend.Static
import qualified Shpadoinkle.Html               as H

import           Types


toFile :: Piece -> ByteString -> File
toFile p bs = File
  { fileGetSize = fromIntegral $ BS.length bs
  , fileToResponse = \status headers -> responseLBS status headers bs
  , fileName = p
  , fileGetHash = pure Nothing
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
      (LRNotFound, Just [ps'])               -> LRFile $ file ps' html
      (_,          Just [ps']) | [ps'] == ps -> LRFile $ file ps' html
      _                                      -> res


data Options = Options
  { assets :: FilePath
  , port   :: Int
  }


parser :: Parser Options
parser = Options
  <$> strOption   (long "assets" <> short 'a' <> metavar "FILEPATH")
  <*> option auto (long "port"   <> short 'p' <> metavar "PORT" <> showDefault <> value 8080)


getSpaceCraft :: SpaceCraftId -> Handler SpaceCraft
getSpaceCraft = undefined


updateSpaceCraft :: SpaceCraftId -> SpaceCraftUpdate -> Handler ()
updateSpaceCraft = undefined


createSpaceCraft :: SpaceCraftUpdate -> Handler SpaceCraftId
createSpaceCraft = undefined


deleteSpaceCraft :: SpaceCraftId -> Handler ()
deleteSpaceCraft = undefined


app :: FilePath -> Application
app root = serve (Proxy @ (API :<|> SPA)) $ serveApi :<|> serveSpa
  where

  serveApi = getSpaceCraft
        :<|> updateSpaceCraft
        :<|> createSpaceCraft
        :<|> deleteSpaceCraft
        :: Server API

  static = serveDirectoryWith $ defaultSPAServerSettings root $ H.div_ ["WOWZERS"]

  serveSpa = static
        :<|> static
        :<|> const static
        :: Server SPA


main :: IO ()
main = do
  Options {..} <- execParser . info (parser <**> helper) $
    fullDesc <> progDesc "Servant CRUD Example"
             <> header "Space craft manager as an example of Shpadoinkle"
  run port $ app assets
