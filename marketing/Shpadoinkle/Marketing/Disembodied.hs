{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}


module Main where


import           Control.Lens                (Prism', re, (^.))
import           Data.Generics.Labels        ()
import           Servant.API                 (type (:<|>) ((:<|>)))
import           System.Environment          (getArgs)

import           Shpadoinkle                 (Html, JSM)
import           Shpadoinkle.Disembodied     (Disembodied (SiteSpec), writeSite)
import           Shpadoinkle.Lens            (onSum)
import           Shpadoinkle.Run             (Env (Prod))

import           Shpadoinkle.Marketing.Types (Comparison (Comparison), Frontend,
                                              SPA)
import           Shpadoinkle.Marketing.View  (comparisons, fourOhFour, home,
                                              template)


wrap :: Applicative m => Prism' Frontend a -> (a -> Html m a) -> a -> b -> Html m Frontend
wrap l v x = const $ template Prod (x ^. re l) (l `onSum` v x)


site :: Applicative m => SiteSpec () (SPA m)
site = wrap #_HomeM home ()
  :<|> wrap #_ComparisonM comparisons . (`Comparison` Nothing)
  :<|> const fourOhFour
  :<|> wrap #_HomeM home ()


main :: IO ()
main = do
  args <- getArgs
  let out = case args of
              [ "--out", out' ] -> out'
              [ "-o", out' ]    -> out'
              _                 -> error "You must pass --out or -o"
  writeSite @ (SPA JSM) out () site
