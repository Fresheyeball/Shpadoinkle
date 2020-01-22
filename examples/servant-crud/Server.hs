{-# LANGUAGE RecordWildCards #-}


module Main where


import           Network.Wai
import           Network.Wai.Handler.Warp
import           Options.Applicative
import           Servant.API
import           Servant.Server
import           Servant.Server.StaticFiles

import           Types


data Options = Options
  { assets :: FilePath
  , port   :: Int
  }


parser :: Parser Options
parser = Options
  <$> strOption   (long "assets" <> short 'a' <> metavar "FILEPATH")
  <*> option auto (long "port"   <> short 'p' <> metavar "PORT" <> showDefault <> value 8081)


getSpaceCraft :: SpaceCraftId -> Handler SpaceCraft
getSpaceCraft = undefined


updateSpaceCraft :: SpaceCraftId -> SpaceCraftUpdate -> Handler ()
updateSpaceCraft = undefined


createSpaceCraft :: SpaceCraftUpdate -> Handler SpaceCraftId
createSpaceCraft = undefined


deleteSpaceCraft :: SpaceCraftId -> Handler ()
deleteSpaceCraft = undefined


app :: FilePath -> Application
app path = serve (addStatic api) $ serveDirectoryWebApp path
  :<|> getSpaceCraft
  :<|> updateSpaceCraft
  :<|> createSpaceCraft
  :<|> deleteSpaceCraft


main :: IO ()
main = do
  Options {..} <- execParser . info (parser <**> helper) $
    fullDesc <> progDesc "Servant CRUD Example" <> header "Space craft manager as an example of Shpadoinkle"
  run port $ app assets
