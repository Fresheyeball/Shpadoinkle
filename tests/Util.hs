{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates      #-}


module Util where


import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
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
  , headless   :: Bool
  } deriving Show


getOptions :: IO Options
getOptions = Options
  <$> getEnv "COMPILER"
  <*> getEnv "EXAMPLES"
  <*> lookupEnv "CHROME"
  <*> lookupEnv "DATADIR"
  <*> ((==) (Just "1") <$> lookupEnv "HEADLESS")


isJS :: String -> Bool
isJS = isInfixOf "js" . pack


serve :: String -> IO () -> IO ()
serve package test = do
  ops@Options {..} <- getOptions
  -- print ops
  if isJS compiler

    then do
      let serving = path <> "/bin/" <> package <> ".jsexe/"
      putStrLn $ "Serving -> " ++ serving
      delay >> delay
      thread <- forkIO . run port . staticApp $ defaultWebAppSettings serving
      test
      killThread thread

    else do
      let exe = path <> "/bin/" <> package
      putStrLn $ "Running -> " ++ exe
      delay >> delay
      handle <- runCommand exe
      test
      terminateProcess handle


hang :: MonadIO m => m ()
hang = liftIO . forever $ threadDelay maxBound


delay :: MonadIO m => m ()
delay = liftIO . threadDelay $ 500 * 1000


port :: Int
port = 8080


sendKeysSlowly :: Text -> Element -> WD ()
sendKeysSlowly ks elm = forM_ (unpack ks) $
  \k -> sendKeys (pack [k]) elm


chrome' :: Maybe FilePath -> Maybe FilePath -> Bool -> Browser
chrome' chromePath dataDir headless = chrome
  { chromeBinary = chromePath
  , chromeOptions = [ "--headless" | headless ] <>
      [ "--no-sandbox"
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
    runSession (useBrowser (chrome' chromePath dataDir headless) defaultConfig) $ do
      openPage $ "http://localhost:" <> show port <> if isJS compiler
        then "/index.html"
        else ""
      delay
      test
      closeSession


expectText :: Element -> Text -> WD ()
expectText e t = do
  t' <- getText e
  if t' == t then return () else t' `equals` t


equals :: (Show a, Eq a) => a -> a -> WD ()
equals x y = liftIO $ x `shouldBe` y


expectClass :: Element -> Text -> WD ()
expectClass e t = attr e "class" >>= equals (Just t)


times :: Applicative m => Int -> m () -> m ()
times i m = () <$ traverse (const m) [1..i]

