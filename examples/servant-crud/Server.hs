{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}


module Main where


import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.FileEmbed
import           Data.Proxy
import           Data.Text.Encoding
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.SQLite.Simple
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Options.Applicative
import           Servant.API
import           Servant.Server

import           GHC.IO.Exception          (IOException)

import           Shpadoinkle
import           Shpadoinkle.Router.Server

import           Types
import           View


data Options = Options
  { assets :: FilePath
  , port   :: Int
  }


newtype App a = App { runApp :: ReaderT Connection IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Connection, MonadError IOException)


toHandler :: MonadIO m => Connection -> App ~> m
toHandler c a = liftIO $ runReaderT (runApp a) c


parser :: Parser Options
parser = Options
  <$> strOption   (long "assets" <> short 'a' <> metavar "FILEPATH")
  <*> option auto (long "port"   <> short 'p' <> metavar "PORT" <> showDefault <> value 8080)


options :: ParserInfo Options
options = info (parser <**> helper) $
    fullDesc <> progDesc "Servant CRUD Example"
             <> header "Space craft manager as an example of Shpadoinkle"


runSql :: (MonadIO m, MonadReader Connection m) => SqliteM ~> m
runSql x = do conn <- ask; liftIO $ runBeamSqlite conn x


newtype Noop a = Noop (JSM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadJSM)
  deriving anyclass CRUDSpaceCraft


instance CRUDSpaceCraft App where

  listSpaceCraft =
    runSql . runSelectReturningList . select . all_ $ _roster db

  getSpaceCraft i =
    runSql . runSelectReturningOne . select
           . filter_ (\s -> val_ (SpaceCraftKey i) ==. primaryKey s) . all_ $ _roster db

  updateSpaceCraft _ _ = error "TODO"
  deleteSpaceCraft     = error "TODO"

  createSpaceCraft SpaceCraftUpdate {..} = do
    xs <- runSql . runInsertReturningList . insert (_roster db) $ insertExpressions
      [ SpaceCraft default_ (val_ _sku) (val_ _description) (val_ _serial) (val_ _squadron) (val_ _operable) ]
    case xs of
      [x] -> return $ _identity x
      _   -> throwError $ userError "Failed to insert"


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
      i <- start r; return . template i $ view @ Noop i) routes


main :: IO ()
main = do
  Options {..} <- execParser options
  conn <- open "roster.db"
  execute_ conn . Query $ decodeUtf8 $(embedFile "./servant-crud/migrate.sql")
  run port $ app conn assets
