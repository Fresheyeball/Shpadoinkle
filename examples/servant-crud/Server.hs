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


module Server where


import           Control.Monad.Reader      (MonadIO, MonadReader (..),
                                            ReaderT (..), liftIO)
import           Data.FileEmbed            (embedFile)
import           Data.Proxy                (Proxy (..))
import           Data.Text.Encoding        (decodeUtf8)
import           Database.Beam             (all_, default_, delete, filter_,
                                            insert, insertExpressions,
                                            primaryKey, runDelete,
                                            runSelectReturningList,
                                            runSelectReturningOne, runUpdate,
                                            save, select, val_, (==.))
import           Database.Beam.Sqlite      (SqliteM, runBeamSqlite,
                                            runInsertReturningList)
import           Database.SQLite.Simple    (Connection, Query (..), execute_,
                                            open)
import           Network.Wai               (Application)
import           Network.Wai.Handler.Warp  (run)
import           Options.Applicative       (Parser, ParserInfo, auto,
                                            execParser, fullDesc, header,
                                            helper, info, long, metavar, option,
                                            progDesc, short, showDefault,
                                            strOption, value, (<**>))
import           Servant.API
import           Servant.Server            (Server, hoistServer, serve)

import           Shpadoinkle               (JSM, type (~>))
import           Shpadoinkle.Router        (MonadJSM)
import           Shpadoinkle.Router.Server (serveUI)
import           Shpadoinkle.Run           (Env (Prod))

import           Types
import           View


data Options = Options
  { assets :: FilePath
  , port   :: Int
  }


newtype App a = App { runApp :: ReaderT Connection IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Connection)


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

  updateSpaceCraft i SpaceCraftUpdate {..} =
    runSql . runUpdate $ save (_roster db) $
      SpaceCraft i _sku _description _serial _squadron _operable

  deleteSpaceCraft i =
    runSql . runDelete $ delete (_roster db) $ \sc -> val_ (SpaceCraftKey i) ==. primaryKey sc

  createSpaceCraft SpaceCraftUpdate {..} =
    runSql . fmap (_identity . head) . runInsertReturningList . insert (_roster db) $ insertExpressions
        [ SpaceCraft default_ (val_ _sku) (val_ _description) (val_ _serial) (val_ _squadron) (val_ _operable) ]


app :: Env -> Connection -> FilePath -> Application
app ev conn root = serve (Proxy @ (API :<|> SPA App)) $ serveApi :<|> serveSpa
  where

  serveApi :: Server API
  serveApi = hoistServer (Proxy @ API) (toHandler conn)
       $ listSpaceCraft
    :<|> getSpaceCraft
    :<|> updateSpaceCraft
    :<|> createSpaceCraft
    :<|> deleteSpaceCraft

  serveSpa :: Server (SPA App)
  serveSpa = serveUI @ (SPA App) root
    (\r -> toHandler conn $ do
      i <- start r; return . template ev i $ view @ Noop i) routes


application :: Env -> FilePath -> IO Application
application ev assets = do
  conn <- open "roster.db"
  execute_ conn . Query $ decodeUtf8 $(embedFile "./servant-crud/migrate.sql")
  return $ app ev conn assets


main :: IO ()
main = do
  Options {..} <- execParser options
  run port =<< application Prod assets

