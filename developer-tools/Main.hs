{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where


import           Control.Lens                ((^.))
import           Control.Monad               (void, when)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Map                    as Map (Map, insert, toList)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text, pack)
import           Data.Time                   (UTCTime, defaultTimeLocale,
                                              formatTime, getCurrentTime)
import           Language.Javascript.JSaddle (FromJSVal (fromJSVal), JSM,
                                              JSString, MonadJSM, fun, js, js1,
                                              js2, jsg, liftJSM, obj,
                                              strictEqual, (<#))
import           Prelude                     hiding (span)
import           UnliftIO                    (TVar, atomically, modifyTVar,
                                              newTVarIO)

import           Shpadoinkle                 (Html, shpadoinkle, text)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html            (getBody, h1_, li, onClickM, span,
                                              ul_)
import           Shpadoinkle.Run             (runJSorWarp)


default (JSString)


newtype History = History { unHistory :: Text }
  deriving (Eq, Ord, Show)


type Model = Map UTCTime History


listenForOutput :: TVar Model -> JSM ()
listenForOutput model = void $ jsg "chrome" ^. js "runtime" ^. js "onMessage" ^. js1 "addListener" (fun $ \_ _ args -> do
  let x = Prelude.head args
  t <- x ^. js "type"
  isRight <- strictEqual t "shpadoinkle_output_state"
  when isRight $ do
    msg <- x ^. js "msg"
    now <- liftIO getCurrentTime
    history <- History . fromMaybe (error "how could this not be a string") <$> fromJSVal msg
    atomically . modifyTVar model $ insert now history)


row :: MonadJSM m => UTCTime -> History -> Html m a
row k history = li [ onClickM . liftJSM $ id <$ sendHistory history ]
  [ span "time" [ text . pack $ formatTime defaultTimeLocale "%H:%M" k ]
  , span "val"  [ text $ unHistory history ]
  ]


sendHistory :: History -> JSM ()
sendHistory (History history) = void $ do
  tabId <- jsg "chrome" ^. js "devtools" ^. js "inspectedWindow" ^. js "tabId"

  msg <- obj
  (msg <# "type") "shpadoinkle_set_state"
  (msg <# "msg") history

  void $ jsg "chrome" ^. js "tabs" ^. js2 "sendMessage" tabId msg


panel :: MonadJSM m => Model -> Html m Model
panel m | m == mempty = h1_ [ "No State Yet" ]
panel m = ul_ $ uncurry row <$> toList m


app :: JSM ()
app = do
  model <- liftIO $ newTVarIO mempty
  listenForOutput model
  shpadoinkle id runParDiff mempty model panel getBody


main :: IO ()
main = runJSorWarp 8080 app

