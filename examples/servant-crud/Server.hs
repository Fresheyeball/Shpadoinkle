{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}


module Main where


import           Control.Monad.IO.Class
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


listSpaceCraft :: IO [SpaceCraft]
listSpaceCraft = return
  [ SpaceCraft 2 0 (Just "thang") 1 AwayTeam Operational
  , SpaceCraft 3 0 (Just "sweet") 2 Scout    Operational
  , SpaceCraft 4 1 (Just "hey")   3 Scout    Inoperable
  ]


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

  serveApi = liftIO listSpaceCraft
        :<|> getSpaceCraft
        :<|> updateSpaceCraft
        :<|> createSpaceCraft
        :<|> deleteSpaceCraft
        :: Server API

  serveSpa = serveUI @SPA root (return . template . view @JSM . start) routes


main :: IO ()
main = do
  Options {..} <- execParser . info (parser <**> helper) $
    fullDesc <> progDesc "Servant CRUD Example"
             <> header "Space craft manager as an example of Shpadoinkle"
  run port $ app assets
