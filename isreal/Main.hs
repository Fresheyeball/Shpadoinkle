{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}


{-
  This file is part of Shpadoinkle Isreal.

  Shpadoinkle Isreal is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Shpadoinkle Isreal is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Shpadoinkle Isreal.  If not, see <https://www.gnu.org/licenses/>.
-}


module Main where


import           Control.Monad            (unless)
import           Control.Monad.IO.Class   (MonadIO (..))
import           Data.ByteString.Lazy     as BSL (writeFile)
import           Data.String              (fromString)
import           Data.Text                as T (Text, intercalate, unpack)
import           Data.Text.Lazy           as T (fromStrict, pack, unlines)
import qualified Data.Text.Lazy.IO        as T
import           Network.Wai.Handler.Warp (run)
import           Servant
import qualified System.Directory         as Dir
import           System.Environment       (getEnv)
import           System.Exit              (ExitCode (ExitFailure, ExitSuccess))
import           System.FilePath          ((</>))
import           System.Process           (proc, readCreateProcessWithExitCode)


import           Shpadoinkle.Isreal.Types


getDir :: Options -> SnowToken -> FilePath
getDir Options {..} snow = territory </> T.unpack (unSnowToken snow)


compile :: MonadIO m => Options -> SnowToken -> SnowNonce -> Code -> m (Either CompileError Text)
compile options@Options {..} snow nonce (Code code) = liftIO $ do
  let dir = getDir options snow
  Dir.createDirectoryIfMissing False dir
  BSL.writeFile (dir </> "Main.hs") code
  isCabal <- Dir.doesFileExist $ dir </> "swan.cabal"
  unless isCabal $ Dir.createFileLink (swan </> "swan.cabal") $ dir </> "swan.cabal"
  Dir.setCurrentDirectory dir
  (exit, _, err) <- readCreateProcessWithExitCode (proc "cabal" ["build", "--ghcjs"]) ""
  case exit of
    ExitSuccess   -> do
      T.writeFile (dir </> artifactPath </> "index.html") mkIndex
      return $ Right "Compiled!"
    ExitFailure _ -> return . Left . CompileError $ fromString err

  where
  mkIndex = T.unlines
    [ "<!DOCTYPE html>"
    , "<html>"
    , "  <head>"
    , "    <script language=\"javascript\" src=\"rts.js\"></script>"
    , "    <script language=\"javascript\" src=\"lib.js\"></script>"
    , "    <script language=\"javascript\" src=\"out.js"
      <> "?token=" <> fromStrict (toQueryParam snow)
      <> "&nonce=" <> pack (show nonce) <> "\"></script>"
    , "  </head>"
    , "  <body>"
    , "  </body>"
    , "  <script language=\"javascript\" src=\"runmain.js\" defer></script>"
    , "</html>"
    ]


clean :: MonadIO m => Options -> SnowToken -> m Text
clean options snow = liftIO $ do
  Dir.removePathForcibly $ getDir options snow
  return $ unSnowToken snow <> " is clean"


cleanAll :: MonadIO m => Options -> m Text
cleanAll Options {..} = liftIO $ do
  Dir.removePathForcibly territory
  Dir.createDirectory territory
  return "All is clean in Colorado"


static :: Options -> SnowToken -> ServerT Raw m
static options snow = serveDirectoryWebApp $ getDir options snow </> artifactPath


artifactPath :: FilePath
artifactPath = "dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/swan-0.1.0.0/x/swan/build/swan/swan.jsexe/"


welcome :: Text
welcome = intercalate "\n"
  [ "Isreal Swan is a microservice for processing haskell files, and serving GHCJS artifacts."
  , "There is no UI."
  , "To learn more please visit the README.md located here: https://gitlab.com/platonic/shpadoinkle/-/blob/master/isreal/README.md"
  ]


api :: Int -> Options -> IO ()
api port options = run port $ Servant.serve (Proxy @API) $ pure
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
