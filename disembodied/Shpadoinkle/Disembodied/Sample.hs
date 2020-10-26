{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}


module Shpadoinkle.Disembodied.Sample where


import           Data.Text               (Text)
import           Servant.API

import           Shpadoinkle             (Html, JSM, MonadJSM, text)
import           Shpadoinkle.Disembodied (Disembodied (SiteSpec), writeSite)
import           Shpadoinkle.Html        (button, h1_, onClick)
import           Shpadoinkle.Router      (View)


type Pages m
  = "about" :> View m Int
  :<|> View m ()


newtype Context = Context
  { siteName :: Text }


about :: MonadJSM m => Int -> Context -> Html m Int
about x ctx =
  h1_ [ text $ "about us at " <> siteName ctx
      , button
        [ onClick (x + 1) ]
        [ "Increment" ]
      ]


home :: Html m a
home = h1_ [ "home" ]


site :: MonadJSM m => SiteSpec Context (Pages m)
site = about 0 :<|> const home


makeSite :: IO ()
makeSite = writeSite @(Pages JSM) "" (Context "Sample") site
