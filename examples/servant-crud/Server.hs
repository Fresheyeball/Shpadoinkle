{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}


module Main where


import           Data.Proxy
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Options.Applicative
import           Servant.API
import           Servant.Server

import           Shpadoinkle
import           Shpadoinkle.Router.Server

import           Types
import           View


data Options = Options
  { assets :: FilePath
  , port   :: Int
  }


parser :: Parser Options
parser = Options
  <$> strOption   (long "assets" <> short 'a' <> metavar "FILEPATH")
  <*> option auto (long "port"   <> short 'p' <> metavar "PORT" <> showDefault <> value 8080)


listSpaceCraft :: Handler [SpaceCraft]
listSpaceCraft = return []


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
updateSpaceCraft _ _ = return ()


createSpaceCraft :: SpaceCraftUpdate -> Handler SpaceCraftId
createSpaceCraft _ = return $ SpaceCraftId 0


deleteSpaceCraft :: SpaceCraftId -> Handler ()
deleteSpaceCraft = undefined


app :: FilePath -> Application
app root = serve (Proxy @ (API :<|> SPA)) $ serveApi :<|> serveSpa
  where

  serveApi = listSpaceCraft
        :<|> getSpaceCraft
        :<|> updateSpaceCraft
        :<|> createSpaceCraft
        :<|> deleteSpaceCraft
        :: Server API

  serveSpa = serveUI @SPA root (template . view @JSM . start) routes


main :: IO ()
main = do
  Options {..} <- execParser . info (parser <**> helper) $
    fullDesc <> progDesc "Servant CRUD Example"
             <> header "Space craft manager as an example of Shpadoinkle"
  run port $ app assets
