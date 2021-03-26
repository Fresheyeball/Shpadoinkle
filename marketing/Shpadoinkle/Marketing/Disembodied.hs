{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}


module Main where


import           Control.Lens                (Prism', re, (^.))
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Generics.Labels        ()
import           Servant.API                 (type (:<|>) ((:<|>)))
import           System.Environment          (getArgs)

import           Shpadoinkle                 (Html, JSM, MonadJSM, TVar)
import           Shpadoinkle.Disembodied     (Disembodied (SiteSpec), writeSite)
import           Shpadoinkle.Isreal.Types    (Code, SnowToken)
import           Shpadoinkle.Lens            (onSum)
import           Shpadoinkle.Run             (Env (Prod))

import           Shpadoinkle.Marketing.Types
import           Shpadoinkle.Marketing.View  (comparisons, fourOhFour,
                                              genExampleTokens, home, template)

newtype Noop a = Noop (JSM a)
  deriving newtype (Functor, Applicative, Monad, MonadJSM, MonadIO)
  deriving anyclass (Hooglable, Swan, MonadReader (TVar (Maybe Code)))


wrap :: Monad m => Prism' Frontend a -> (a -> Html m a) -> a -> b -> Html m Frontend
wrap l v x = const $ template Prod (x ^. re l) (l `onSum` v x)


site :: Hooglable m => ExampleEffects m => MonadJSM m => Examples SnowToken -> SiteSpec () (SPA m)
site token = wrap #_HomeM home (emptyHome token)
  :<|> wrap #_ComparisonM comparisons . (`Comparison` Nothing)
  :<|> const fourOhFour
  :<|> wrap #_HomeM home (emptyHome token)


main :: IO ()
main = do
  args <- getArgs
  let out = case args of
              [ "--out", out' ] -> out'
              [ "-o", out' ]    -> out'
              _                 -> error "You must pass --out or -o"
  token <- genExampleTokens
  writeSite @ (SPA Noop) out () (site token)
