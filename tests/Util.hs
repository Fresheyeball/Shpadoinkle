{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates      #-}


module Util where


import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text
import           GHC.Conc
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           System.Environment
import           System.Process

import           Test.Hspec
import           Test.WebDriver

import           Debug.Trace                    (trace)


data Options = Options
  { compiler   :: String
  , path       :: String
  , chromePath :: Maybe FilePath
  , dataDir    :: Maybe FilePath
  } deriving Show


getOptions :: IO Options
getOptions = Options <$> getEnv "COMPILER" <*> getEnv "EXAMPLES" <*> lookupEnv "CHROME" <*> lookupEnv "DATADIR"


serve :: String -> IO () -> IO ()
serve package test = do
  Options {..} <- getOptions
  case compiler of


    "ghc843" -> do
      let exe = path <> "/bin/" <> package
      handle <- runCommand exe
      test
      terminateProcess handle


    "ghcjs84" ->  do
      let serving = path <> "/bin/" <> package <> ".jsexe/"
      thread <- forkIO . run port . staticApp $ defaultWebAppSettings serving
      test
      killThread thread


hang :: MonadIO m => m ()
hang = liftIO . forever $ threadDelay maxBound


delay :: MonadIO m => m ()
delay = liftIO $ threadDelay 100000


port :: Int
port = 8080


sendKeysSlowly :: Text -> Element -> WD ()
sendKeysSlowly ks elm = forM_ (unpack ks) $
  \k -> sendKeys (pack [k]) elm


chrome' :: Maybe FilePath -> Maybe FilePath -> Browser
chrome' chromePath dataDir = chrome
  { chromeBinary = chromePath
  , chromeOptions =
      [ "--no-sandbox"
      , "--headless"
      , "--profile-directory=Default"
      , "--disable-gpu"
      , "--window-size=1024,768"
      , "--disable-notifications"
      , "--disable-dev-shm-usage"
      ] <> maybe [] (pure . ("--user-data-dir=" <>)) dataDir
  }


itWD :: String -> WD () -> Spec
itWD should test =
  it should $ do
    opts@Options {..} <- getOptions
    runSession (useBrowser (chrome' chromePath dataDir) defaultConfig) $ do
      openPage $ "http://localhost:" <> show port <> case compiler of
        "ghcjs84" -> "/index.html"
        "ghc843"  -> ""
      delay
      test
      closeSession


expectText :: Element -> Text -> WD ()
expectText e t = do
  t' <- getText e
  if t' == t then return () else liftIO $ t' `shouldBe` t


equals :: (Show a, Eq a) => a -> a -> WD ()
equals x y = liftIO $ x `shouldBe` y
