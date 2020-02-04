{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}


module Main where


import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.FileEmbed
import           Data.Proxy
import           Data.Set                  as Set
import           Data.Text.Encoding
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.SQLite.Simple
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


newtype App a = App { runApp :: ReaderT Connection IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Connection)


toHandler :: MonadIO m => Connection -> App a -> m a
toHandler c a = liftIO $ runReaderT (runApp a) c


parser :: Parser Options
parser = Options
  <$> strOption   (long "assets" <> short 'a' <> metavar "FILEPATH")
  <*> option auto (long "port"   <> short 'p' <> metavar "PORT" <> showDefault <> value 8080)


runSql :: (MonadIO m, MonadReader Connection m) => SqliteM b -> m b
runSql x = do conn <- ask; liftIO $ runBeamSqlite conn x


instance CRUDSpaceCraft App where

  listSpaceCraft =
    fmap Set.fromList . runSql . runSelectReturningList . select . all_ $ _roster db

  getSpaceCraft i =
    runSql . runSelectReturningOne . select
           . filter_ (\s -> val_ (SpaceCraftKey i) ==. primaryKey s) . all_ $ _roster db

  updateSpaceCraft _ _ = error "TODO"
  deleteSpaceCraft     = error "TODO"
  createSpaceCraft _   = error "TODO"


app :: Connection -> FilePath -> Application
app conn root = serve (Proxy @ (API :<|> SPA)) $ serveApi :<|> serveSpa
  where

  serveApi :: Server API
  serveApi = hoistServer (Proxy @ API) (toHandler conn)
       $ listSpaceCraft
    :<|> getSpaceCraft
    :<|> updateSpaceCraft
    :<|> createSpaceCraft
    :<|> deleteSpaceCraft

  serveSpa :: Server SPA
  serveSpa = serveUI @ SPA root
    (\r -> toHandler conn $ do
      i <- start r; return . template i $ view @ JSM i) routes


main :: IO ()
main = do
  Options {..} <- execParser . info (parser <**> helper) $
    fullDesc <> progDesc "Servant CRUD Example"
             <> header "Space craft manager as an example of Shpadoinkle"
  conn <- open "roster.db"
  execute_ conn . Query $ decodeUtf8 $(embedFile "./servant-crud/migrate.sql")
  run port $ app conn assets
