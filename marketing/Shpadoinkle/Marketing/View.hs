{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}

{-# OPTIONS_GHC -fno-warn-type-defaults   #-}


module Shpadoinkle.Marketing.View where


import           Control.Lens                        (to, (%~), (<>~), (?~),
                                                      (^.))
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Data.Generics.Labels                ()
import           Data.String
import           Data.Text                           (Text, pack)
import           Data.Text.Lazy                      (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding             (decodeUtf8, encodeUtf8)
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.WindowOrWorkerGlobalScope (setTimeout)
import           Language.Javascript.JSaddle
import           Prelude                             hiding (div)
import           Servant                             (toUrlPiece)
import           Shpadoinkle
import           Shpadoinkle.Console                 as Console
import           Shpadoinkle.Html                    as H
import           Shpadoinkle.Isreal.Types            as Swan
import           Shpadoinkle.Lens                    (onRecord, onRecordEndo,
                                                      onSum)
import qualified Shpadoinkle.Marketing.Tailwind      as T
import           Shpadoinkle.Router                  (toHydration)
import           Shpadoinkle.Run                     (Env, entrypoint)
import           Shpadoinkle.Widgets.Form.Dropdown
import qualified Shpadoinkle.Widgets.Form.Input      as I
import           Shpadoinkle.Widgets.Types           (Pick (One), Search (..),
                                                      withOptions)

import           Shpadoinkle.Marketing.Types
import           Shpadoinkle.Marketing.Types.Hoogle


default (Text)


domain :: IsString s => s
domain = "https://shpadoinkle.org"


hero :: Html m a
hero =
  div
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
  div
    [ class' $ T.mx_auto <> T.text_white <> T.max_w_3xl <> T.my_5
            <> T.flex <> T.space_x_4 <> T.justify_between
    ]
    [ div
      [ class' $ T.bg_gray_900 <> T.p_4 <> "w-1/3" ]
      [ h3_ [ "Declarative" ]
      , "Because types!"
      ]

    , div
      [ class' $ T.bg_gray_900 <> T.p_4 <> "w-1/3" ]
      [ h3_ [ "Modular" ]
      , "Because packages!"
      ]

    , div
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
    [ div
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


mirrorCfg :: Code -> JSM Object
mirrorCfg (Code cc) = do
  cfg <- obj
  (cfg <# "mode") "haskell"
  (cfg <# "value") $ toStrict $ decodeUtf8 cc
  (cfg <# "indentUnit") (4 :: Int)
  (cfg <# "matchBrackets") True
  return cfg


mirror :: Code -> Html m Code
mirror cc = baked $ do
  (notify, stream) <- mkGlobalMailboxAfforded constUpdate
  doc <- currentDocumentUnchecked
  container <- toJSVal =<< createElement doc "div"
  cfg <- mirrorCfg cc
  cm <- jsg2 "CodeMirror" container cfg
  _ <- cm ^. js2 "on" "change" (fun $ \_ _ _ -> do
        jsv <- cm ^. js0 "getValue"
        raw :: Maybe Text <- fromJSVal jsv
        maybe (pure ()) (notify . Code . encodeUtf8 . fromStrict) raw
      )
  window <- currentWindowUnchecked
  _ <- setTimeout window (fun $ \_ _ _ -> () <$ cm ^. js0 "refresh") (Just 33)
  return (RawNode container, stream)


example :: MonadJSM m => Swan m => Example -> Html m Example
example (Example cc token merr nonce') = div "example"
  [ mapC (\ccUpdate -> before (onRecord #inputHaskell ccUpdate) $ impur $ do
     Console.log @ToJSVal "call compile"
     cc' <- runContinuation ccUpdate cc
     res <- compile token $ exampleTemplate $ cc' cc
     Console.log @Show res
     return $ (#nonce %~ (+ 1)) . case res of
       Left e -> #err ?~ e
       _      -> id

    ) $ mirror cc
  , case merr of
      Nothing -> iframe
        [ src $ "https://isreal.shpadoinkle.org/"
             <> toUrlPiece (Swan.serve token)
             <> "/index.html?nonce="
             <> pack (show nonce')
        ] []
      Just e -> div_ [ text $ pack $ show e ]
  ]


hoogleWidget :: forall m. Hooglable m => MonadJSM m => Hoogle -> Html m Hoogle
hoogleWidget hoo =
  div
  [ onInputM (query . Search) ]
  [ onRecord #search $ I.search [] (search hoo)
  , onRecord #targets $ div [ class' T.p_2 ] [ dropdown theme defConfig $ targets hoo ]
  ]

 where

 query :: Search -> m (Hoogle -> Hoogle)
 query ss = do
   ts <- findTargets ss
   return $ #targets <>~ Nothing `withOptions` ts


 theme :: Dropdown 'One Target -> Theme m 'One Target
 theme _ = Theme div_ (const mempty) div_ targetWidget


targetWidget :: Target -> Html m a
targetWidget = div' . pure . innerHTML . pack . targetItem


home :: Hooglable m => Swan m => MonadJSM m => Home -> Html m Home
home home' = section_
  [ onRecordEndo #hoogle top home'
  , hero
  , pitch
  , onRecordEndo (#examples . #helloWorld) example home'
  ]


comparisons :: Comparison -> Html m Comparison
comparisons x = h1_ [ text $ "Comparison with, " <> x ^. #framework . to (pack . show) ]


fourOhFour :: Html m a
fourOhFour = h2_ [ "404" ]


view :: Hooglable m => Swan m => MonadJSM m => Frontend -> Html m Frontend
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
    , H.link'
      [ H.rel "stylesheet"
      , H.href $ codemirrorCDN "codemirror.min.css"
      ]
    , H.meta [ H.charset "ISO-8859-1" ] []
    , toHydration fe
    , H.script [ H.src $ entrypoint ev ] []
    , H.script [ H.src $ codemirrorCDN "codemirror.min.js" ] []
    , H.script [ H.src $ codemirrorCDN "mode/haskell/haskell.min.js" ] []
    ]
  , H.body_
    [ v
    ]
  ]
    where codemirrorCDN = mappend "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.59.4/"


start :: MonadIO m => Route -> m Frontend
start = \case
  HomeR         -> HomeM . emptyHome <$> liftIO genSnowToken
  ComparisonR f -> pure . ComparisonM $ Comparison f Nothing
  FourOhFourR   -> pure FourOhFourM
