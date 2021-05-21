{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}


module Shpadoinkle.Disembodied.Sample where


import           Servant.API

import           Shpadoinkle             (Html, JSM, MonadJSM)
import           Shpadoinkle.Disembodied (Disembodied (SiteSpec), writeSite)
import           Shpadoinkle.Html        (button, h1_, onClick)
import           Shpadoinkle.Router      (View)


type Pages m
  = "about" :> View m Int
  :<|> View m ()


about :: MonadJSM m => Html m Int
about =
  h1_ [ button
        [ onClick (+ 1) ]
        [ "Increment" ]
      ]


home :: Monad m => Html m a
home = h1_ [ "home" ]


site :: MonadJSM m => SiteSpec (Pages m)
site = about :<|> home


makeSite :: IO ()
makeSite = writeSite @(Pages JSM) "" site
