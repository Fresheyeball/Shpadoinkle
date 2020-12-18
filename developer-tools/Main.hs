{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where


import           Control.Lens                ((^.))
import           Control.Monad               (void, when)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Map                    as Map (Map, insert, toList)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text, pack, unpack)
import           Data.Time                   (UTCTime, defaultTimeLocale,
                                              formatTime, getCurrentTime)
import           Language.Javascript.JSaddle (FromJSVal (fromJSVal), JSM,
                                              JSString, MonadJSM, fun, js, js1,
                                              js2, jsg, liftJSM, obj,
                                              strictEqual, (<#))
import           Prelude                     hiding (div, span)
import qualified Text.Show.Pretty            as Pretty
import           UnliftIO                    (TVar, atomically, modifyTVar,
                                              newTVarIO)

import           Shpadoinkle                 (Html, flagProp, shpadoinkle, text)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html
import           Shpadoinkle.Run             (runJSorWarp)


default (JSString)


newtype History = History { unHistory :: Text }
  deriving (Eq, Ord, Show)


type Model = Map UTCTime History


listenForOutput :: TVar Model -> JSM ()
listenForOutput model = void $ jsg "chrome" ^. (js "runtime" . js "onMessage" . js1 "addListener" (fun $ \ _ _ args -> do
  let x = Prelude.head args
  t <- x ^. js "type"
  isRight <- strictEqual t "shpadoinkle_output_state"
  when isRight $ do
    msg <- x ^. js "msg"
    now <- liftIO getCurrentTime
    history <- History
                 . fromMaybe (error "how could this not be a string")
                 <$> fromJSVal msg
    atomically . modifyTVar model $ insert now history))


row :: MonadJSM m => UTCTime -> History -> Html m a
row k history = div "record"
  [ span [ onClickM . liftJSM $ id <$ sendHistory history
         , className "time"
         ]
         [ text . pack $ formatTime defaultTimeLocale "%X" k ]
  , span "val"  [ maybe (text "failed to parse value") prettyHtml $ Pretty.parseValue $ unpack $ unHistory history ]
  ]


sendHistory :: History -> JSM ()
sendHistory (History history) = void $ do
  tabId <- jsg "chrome" ^. (js "devtools" . js "inspectedWindow" . js "tabId")

  msg <- obj
  (msg <# "type") "shpadoinkle_set_state"
  (msg <# "msg") history

  void $ jsg "chrome" ^. (js "tabs" . js2 "sendMessage" tabId msg)


prettyHtml :: Pretty.Value -> Html m a
prettyHtml = \case
  Pretty.Con con [] -> div "con-uniary" $ string con
  Pretty.Con con slots -> details [ className "con-wrap", ("open", flagProp False) ]
    [ summary "con" $ string con
    , div "con-children" $ prettyHtml <$> slots
    ]
  Pretty.Rec rec fields ->
    details [ className "rec-wrap" ]
    [ summary "rec" $ string rec
    , dl "rec" $ (\((n, v), i) ->
        [ dt_ $ string $ n <> " = "
        , dd_ $ [ prettyHtml v ]
        ]) =<< fields
    ]
  Pretty.String s -> div "string" $ string s
  Pretty.Float n -> div "float" $ string n
  Pretty.Integer n -> div "integer" $ string n
  Pretty.Char c -> div "char" $ string c
  _ -> text "NOT YET"
  where string = pure . text . pack


panel :: MonadJSM m => Model -> Html m Model
panel m | m == mempty = h1_ [ "No State Yet" ]
panel m = div_ $ uncurry row <$> toList m


app :: JSM ()
app = do
  model <- liftIO $ newTVarIO mempty
  listenForOutput model
  shpadoinkle id runParDiff mempty model panel getBody


main :: IO ()
main = runJSorWarp 8080 app
