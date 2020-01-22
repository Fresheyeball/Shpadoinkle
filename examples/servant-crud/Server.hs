{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}


module Main where


import           Data.Proxy
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Options.Applicative
import           Servant.API
import           Servant.Server
import           Servant.Server.StaticFiles
import           WaiAppStatic.Types

import           Types


defaultSPAServerSettings :: FilePath -> StaticSettings
defaultSPAServerSettings root = settings { ssLookupFile = orIndex }
  where
  settings   = defaultFileServerSettings root
  orIndex ps = do
    res <- ssLookupFile settings ps
    case (res, toPieces ["index.html"]) of
      (LRNotFound, Just ps') -> ssLookupFile settings ps'
      _                      -> return res


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

  static = serveDirectoryWith $ defaultSPAServerSettings root

  serveSpa = static
        :<|> static
        :<|> const static
        :: Server SPA


main :: IO ()
main = do
  Options {..} <- execParser . info (parser <**> helper) $
    fullDesc <> progDesc "Servant CRUD Example" <> header "Space craft manager as an example of Shpadoinkle"
  run port $ app assets
