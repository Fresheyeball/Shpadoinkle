{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}


module Main where


import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Generics.Labels                        ()
import           Shpadoinkle                                 (JSM, MonadJSM,
                                                              TVar)
import           Shpadoinkle.Disembodied                     (Disembodied (SiteSpec),
                                                              writeSite,
                                                              writeSiteMap)
import           Shpadoinkle.Isreal.Types                    (Code)
import           Shpadoinkle.Router                          (traverseUnions)
import           Shpadoinkle.Run                             (Env (Prod))
import           Shpadoinkle.Website.Partials.Template       (template)
import           Shpadoinkle.Website.Types.CurrentYear       (getCurrentYear)
import           Shpadoinkle.Website.Types.Effects.Example   (ExampleEffects)
import           Shpadoinkle.Website.Types.Effects.Hooglable (Hooglable)
import           Shpadoinkle.Website.Types.Effects.Swan      (Swan)
import           Shpadoinkle.Website.Types.Home              (Examples)
import           Shpadoinkle.Website.Types.SPA               (SPA, routes)
import           Shpadoinkle.Website.View                    as View
import           System.Environment                          (getArgs)


newtype Noop a = Noop (JSM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving anyclass (Hooglable, Swan, MonadReader (Examples (TVar (Maybe Code))))


getStarts :: (MonadJSM m, ExampleEffects m) => IO (SiteSpec (SPA m))
getStarts = do
  cy <- getCurrentYear
  traverseUnions (fmap (template Prod <*> view cy) . start) routes


main :: IO ()
main = do
  args <- getArgs
  let out = case args of
              [ "--out", out' ] -> out'
              [ "-o", out' ]    -> out'
              _                 -> error "You must pass --out or -o"
  site <- getStarts
  writeSiteMap @ (SPA Noop) "https://shpadoinkle.org" out site
  writeSite    @ (SPA Noop) out site
