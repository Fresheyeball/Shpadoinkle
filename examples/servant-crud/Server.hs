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

import           Types
import           View


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
      (LRNotFound, Just [ps'])                           -> LRFile $ file ps' html
      (_,          Just [ps']) | [ps'] == ps || ps == [] -> LRFile $ file ps' html
      _                                                  -> res


data Options = Options
  { assets :: FilePath
  , port   :: Int
  }


parser :: Parser Options
parser = Options
  <$> strOption   (long "assets" <> short 'a' <> metavar "FILEPATH")
  <*> option auto (long "port"   <> short 'p' <> metavar "PORT" <> showDefault <> value 8080)


getSpaceCraft :: SpaceCraftId -> Handler SpaceCraft
getSpaceCraft _ = return $ SpaceCraft
  { _identity    = SpaceCraftId 0
  , _sku         = SKU 23
  , _description = Nothing
  , _serial      = SerialNumber 12312
  , _squadron    = AwayTeam
  , _operable    = Operational
  }


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

  ui = serveDirectoryWith . defaultSPAServerSettings root . template . view @JSM

  serveSpa = ui . start . Echo
        :<|> ui (start Root)
        :<|> ui (start Root)
        :: Server SPA


main :: IO ()
main = do
  Options {..} <- execParser . info (parser <**> helper) $
    fullDesc <> progDesc "Servant CRUD Example"
             <> header "Space craft manager as an example of Shpadoinkle"
  run port $ app assets
