{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}

module Main where


import           Control.Monad            (unless)
import           Control.Monad.IO.Class   (MonadIO (..))
import           Data.ByteString.Lazy     as BSL (writeFile)
import           Data.String              (fromString)
import           Data.Text                as T (Text, intercalate, unpack)
import           Network.Wai.Handler.Warp (run)
import           Servant
import qualified System.Directory         as Dir
import           System.Environment       (getEnv)
import           System.Exit              (ExitCode (ExitFailure, ExitSuccess))
import           System.FilePath          ((</>))
import           System.Process           (proc, readCreateProcessWithExitCode)


import           Shpadoinkle.Isreal.Types


getDir :: Options -> SnowToken -> FilePath
getDir Options {..} (SnowToken snow) = territory </> T.unpack snow


compile :: MonadIO m => Options -> SnowToken -> Code -> m (Either CompileError Text)
compile options@Options {..} snow (Code code) = liftIO $ do
  let dir = getDir options snow
  Dir.createDirectoryIfMissing False dir
  BSL.writeFile (dir </> "Main.hs") code
  isCabal <- Dir.doesFileExist $ dir </> "swan.cabal"
  unless isCabal $ Dir.createFileLink (swan </> "swan.cabal") $ dir </> "swan.cabal"
  Dir.setCurrentDirectory dir
  (exit, _, err) <- readCreateProcessWithExitCode (proc "cabal" ["build", "--ghcjs"]) ""
  return $ case exit of
    ExitSuccess   -> Right "Compiled!"
    ExitFailure _ -> Left . CompileError $ fromString err


clean :: MonadIO m => Options -> SnowToken -> m Text
clean options snow@(SnowToken snow') = liftIO $ do
  Dir.removePathForcibly $ getDir options snow
  return $ snow' <> " is clean"


cleanAll :: MonadIO m => Options -> m Text
cleanAll Options {..} = liftIO $ do
  Dir.removePathForcibly territory
  Dir.createDirectory territory
  return "All is clean in Colorado"


static :: Options -> SnowToken -> ServerT Raw m
static options snow = serveDirectoryWebApp $ getDir options snow </>
  "dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/swan-0.1.0.0/x/swan/build/swan/swan.jsexe/"


welcome :: Text
welcome = intercalate "\n"
  [ "Isreal Swan is a microservice for processing haskell files, and serving GHCJS artifacts."
  , "There is no UI."
  , "To learn more please visit the README.md located here: https://gitlab.com/platonic/shpadoinkle/-/blob/master/isreal/README.md"
  ]


api :: Int -> Options -> IO ()
api port options = run port $ serve (Proxy @API) $ pure
  :<|> compile  options
  :<|> clean    options
  :<|> cleanAll options
  :<|> static   options
  :<|> pure welcome


main :: IO ()
main = do
  Prelude.putStrLn "starting to build snowmen"
  options <- Options <$> getEnv "TERRITORY" <*> getEnv "SWAN"
  port <- read <$> getEnv "PORT"
  api port options
