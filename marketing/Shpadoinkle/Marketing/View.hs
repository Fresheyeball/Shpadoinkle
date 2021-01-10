{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Shpadoinkle.Marketing.View where


import           Control.Lens                       (to, (<>~), (^.))
import           Data.Generics.Labels               ()
import           Data.String
import           Data.Text                          (pack)

import           Shpadoinkle                        (Html, MonadJSM, text)
import           Shpadoinkle.Html                   as H
import           Shpadoinkle.Lens                   (onRecord, onSum)
import qualified Shpadoinkle.Marketing.Tailwind     as T
import           Shpadoinkle.Router                 (toHydration)
import           Shpadoinkle.Run                    (Env, entrypoint)
import           Shpadoinkle.Widgets.Form.Dropdown
import qualified Shpadoinkle.Widgets.Form.Input     as I
import           Shpadoinkle.Widgets.Types          (Pick (One), Search (..),
                                                     withOptions)

import           Shpadoinkle.Marketing.Types
import           Shpadoinkle.Marketing.Types.Hoogle


domain :: IsString s => s
domain = "https://shpadoinkle.org"


hero :: Html m a
hero =
  H.div
    [ class' $ T.w_full <> T.bg_gray_900 <> T.p_10
            <> T.text_center <> T.text_white
    ]
    [ h1
      [ class' $ T.uppercase <> T.tracking_widest <> T.text_6xl <> T.font_thin
      ]
      [ "Shpadoinkle!"
      ]
    , h2 [ class' T.text_2xl ]
      [ "I think I know precisely what I mean" ]
    , button [ class' $ T.px_3 <> T.py_2 <> T.my_5 <> T.bg_blue_500 ] [ "Get Started" ]
    ]


pitch :: Html m a
pitch =
  H.div
    [ class' $ T.mx_auto <> T.text_white <> T.max_w_3xl <> T.my_5
            <> T.flex <> T.space_x_4 <> T.justify_between
    ]
    [ H.div
      [ class' $ T.bg_gray_900 <> T.p_4 <> "w-1/3" ]
      [ h3_ [ "Declarative" ]
      , "Because types!"
      ]

    , H.div
      [ class' $ T.bg_gray_900 <> T.p_4 <> "w-1/3" ]
      [ h3_ [ "Modular" ]
      , "Because packages!"
      ]

    , H.div
      [ class' $ T.bg_gray_900 <> T.p_4 <> "w-1/3" ]
      [ h3_ [ "Performant" ]
      , "Because simple!"
      ]
    ]


top :: Hooglable m => MonadJSM m => Hoogle -> Html m Hoogle
top hoo =
  header
    [ class' $ T.bg_gray_900 <> T.py_2 <> T.px_5
    ]
    [ H.div
      [ class' $ T.text_center <> T.flex
              <> T.items_center <> T.justify_between <> T.max_w_3xl
              <> T.mx_auto
      ]
      [ img' $ let d = 50 in [ src "/static/logo.png", width d, height d ]
      , nav [ class' $ T.text_white <> T.flex <> T.space_x_4 ]
        [ a [ href $ domain <> "/docs/index.html" ] [ "Docs" ]
        , a [ href $ domain <> "/docs/tutorial/index.html" ] [ "Tutorial" ]
        , a [ href "" ] [ "Community" ]
        , hoogleWidget hoo
        ]
      ]
    ]


hoogleWidget :: forall m. Hooglable m => MonadJSM m => Home -> Html m Home
hoogleWidget h =
  H.div
  [ onInputM (query . Search) ]
  [ onRecord #search $ I.search [] (search h)
  , onRecord #targets $ H.div [ class' T.p_2 ] [ dropdown theme defConfig $ targets h ]
  ]

 where

 query :: Search -> m (Home -> Home)
 query ss = do
   ts <- findTargets ss
   return $ #targets <>~ Nothing `withOptions` ts


 theme :: Dropdown 'One Target -> Theme m 'One Target
 theme _ = Theme H.div_ (const mempty) H.div_ targetWidget


targetWidget :: Target -> Html m a
targetWidget = div' . pure . innerHTML . pack . targetItem


home :: Hooglable m => MonadJSM m => Home -> Html m Home
home h = section_
  [ top h
  , hero
  , pitch
  ]


comparisons :: Comparison -> Html m Comparison
comparisons x = h1_ [ text $ "Comparison with, " <> x ^. #framework . to (pack . show) ]


fourOhFour :: Html m a
fourOhFour = h2_ [ "404" ]


view :: Hooglable m => MonadJSM m => Frontend -> Html m Frontend
view = \case
  HomeM x       -> #_HomeM       `onSum` home x
  ComparisonM x -> #_ComparisonM `onSum` comparisons x
  FourOhFourM   -> fourOhFour


template :: Env -> Frontend -> Html m Frontend ->  Html m Frontend
template ev fe v = H.html_
  [ H.head_
    [ H.link'
      [ H.rel "stylesheet"
      , H.href "/static/tailwind.min.css"
      ]
    , H.link'
      [ H.rel "stylesheet"
      , H.href "/static/style.css"
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
  HomeR         -> pure $ HomeM mempty
  ComparisonR f -> pure . ComparisonM $ Comparison f Nothing
  FourOhFourR   -> pure FourOhFourM
