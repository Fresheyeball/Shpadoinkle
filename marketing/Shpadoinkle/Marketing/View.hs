{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}


module Shpadoinkle.Marketing.View where


import           Control.Lens                (to, (^.))
import           Data.Generics.Labels        ()
import           Data.Text                   (pack)

import           Shpadoinkle                 (Html, text)
import           Shpadoinkle.Html            as H (body_, charset, h1_, h2_,
                                                   head_, href, html_, link',
                                                   meta, rel, script, src)
import           Shpadoinkle.Lens            (onSum)
import           Shpadoinkle.Router          (toHydration)
import           Shpadoinkle.Run             (Env, entrypoint)

import           Shpadoinkle.Marketing.Types (Comparison (Comparison),
                                              Frontend (..), Home, Route (..))


home :: Home -> Html m Home
home _ = h1_ [ "Home page" ]


comparisons :: Comparison -> Html m Comparison
comparisons x = h1_ [ text $ "Comparison with, " <> x ^. #framework . to (pack . show) ]


fourOhFour :: Html m a
fourOhFour = h2_ [ "404" ]


view :: Applicative m => Frontend -> Html m Frontend
view = \case
  HomeM x       -> #_HomeM       `onSum` home x
  ComparisonM x -> #_ComparisonM `onSum` comparisons x
  FourOhFourM   -> fourOhFour


template :: Env -> Frontend -> Html m Frontend ->  Html m Frontend
template ev fe v = H.html_
  [ H.head_
    [ H.link'
      [ H.rel "stylesheet"
      , H.href "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.3.1/css/bootstrap.min.css"
      ]
    , H.meta [ H.charset "ISO-8859-1" ] []
    , toHydration fe
    , H.script [ H.src $ entrypoint ev ] []
    ]
  , H.body_
    [ v
    ]
  ]


start :: Applicative m => Route -> m Frontend
start = \case
  HomeR         -> pure $ HomeM ()
  ComparisonR f -> pure . ComparisonM $ Comparison f Nothing
  FourOhFourR   -> pure FourOhFourM
