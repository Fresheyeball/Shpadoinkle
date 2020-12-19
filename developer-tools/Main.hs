{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where


import           Control.Lens
import           Control.Monad               (void, when)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Map                    as Map (Map, insert, lookup,
                                                     toDescList)
import           Data.Text                   (Text, pack, unpack)
import           Data.Time                   (UTCTime, defaultTimeLocale,
                                              formatTime, getCurrentTime)
import           Language.Javascript.JSaddle (FromJSVal (fromJSVal), JSM,
                                              MonadJSM, fun, js, js1, js2, jsg,
                                              liftJSM, obj, strictEqual, (<#))
import           Prelude                     hiding (div, span)
import qualified Text.Show.Pretty            as Pretty
import           UnliftIO                    (TVar, atomically, modifyTVar,
                                              newTVarIO)

import           Shpadoinkle                 (Html, flagProp, shpadoinkle, text)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html
import           Shpadoinkle.Run             (runJSorWarp)


default (Text)


newtype History = History { unHistory :: Text }
  deriving (Eq, Ord, Show)


data Model = Model
  { _history :: Map UTCTime History
  , _active  :: Maybe UTCTime
  , _sync    :: Bool
  } deriving (Eq, Show)
makeLenses ''Model


emptyModel :: Model
emptyModel = Model mempty Nothing True


listenForOutput :: TVar Model -> JSM ()
listenForOutput model = void $ jsg "chrome" ^. (js "runtime" . js "onMessage" . js1 "addListener" (fun $ \ _ _ args -> do
  let x = Prelude.head args
  t <- x ^. js "type"
  isRight <- strictEqual t "shpadoinkle_output_state"
  when isRight $ do
    msg <- x ^. js "msg"
    now <- liftIO getCurrentTime
    history' <- maybe (error "how could this not be a string") History <$> fromJSVal msg
    atomically . modifyTVar model $ heard now history'))


heard :: UTCTime -> History -> Model -> Model
heard now history' m = m & history %~ insert now history' &
  case m ^. active of
    Just _ | m ^. sync -> active ?~ now
    Nothing            -> active ?~ now
    Just _             -> id


row :: MonadJSM m => Maybe UTCTime -> UTCTime -> History -> Html m Model
row sel k history' = div "record"
  [ div [ className "time"
        , class' [("active", sel == Just k)]
        ]
     [ span_ [ text . pack $ formatTime defaultTimeLocale "%X%Q" k ]
     , button [ onClick $ (sync .~ False) . (active ?~ k) ] [ "Inspect" ]
     , button [ onClickM_ . liftJSM $ sendHistory history' ] [ "Send" ]
     ]
  ]


sendHistory :: History -> JSM ()
sendHistory (History history') = void $ do
  tabId <- jsg "chrome" ^. (js "devtools" . js "inspectedWindow" . js "tabId")

  msg <- obj
  (msg <# "type") "shpadoinkle_set_state"
  (msg <# "msg") history'

  void $ jsg "chrome" ^. (js "tabs" . js2 "sendMessage" tabId msg)


prettyHtml :: Int -> Pretty.Value -> Html m a
prettyHtml depth = \case
  Pretty.Con con [] -> div "con-uniary" $ string con
  Pretty.Con con slots -> details [ className "con-wrap", ("open", flagProp $ depth < 3) ]
    [ summary "con" $ string con
    , div (withDepth "con-children") $ prettyHtml (depth + 1) <$> slots
    ]
  Pretty.Rec rec fields ->
    details (withDepth "rec-wrap")
    [ summary "rec" $ string rec
    , dl "rec" $ (\(n, v)->
        [ dt_ $ string $ n <> " = "
        , dd_ [ prettyHtml (depth + 1) v ]
        ]) =<< fields
    ]
  Pretty.Tuple xs  -> prettyHtml depth $ Pretty.Con "(,)" xs
  Pretty.List []   -> prettyHtml depth $ Pretty.Con "[]" []
  Pretty.List xs   -> ul "list" $ li_.pure.prettyHtml (depth +1) <$> xs
  Pretty.String ss -> div "string" $ string ss
  Pretty.Float n   -> div "float" $ string n
  Pretty.Integer n -> div "integer" $ string n
  Pretty.Char c    -> div "char" $ string c
  _ -> text "NOT YET"
  where string = pure . text . pack
        withDepth x = [ class' [ x, "depth-" <> pack (show depth) ] ]


syncState :: Model -> Model
syncState m =
  m & sync .~ True & active .~ (m ^. history . to (g . toDescList))
  where g ((x,_):_) = Just x
        g _         = Nothing


panel :: MonadJSM m => Model -> Html m Model
panel m = div "wrapper"
  [ div "current-state" $ case _active m >>= flip Map.lookup (_history m) of
      Just history' -> [ maybe (text "failed to parse value") (prettyHtml 0) . Pretty.parseValue . unpack $ unHistory history' ]
      _             -> [ "No State" ]
  , div "history" $ button
    [ onClick syncState
    , className "sync-button"
    , class' [ ("sync", m ^. sync) ]
    ] [ "Sync State" ] : (book <> [clear])
  ]
  where book = uncurry (m ^. active . to row) <$> Map.toDescList (m ^. history)
        clear = a [ className "clear", onClick $ const emptyModel ] [ "Clear History" ]


app :: JSM ()
app = do
  model <- liftIO $ newTVarIO emptyModel
  listenForOutput model
  shpadoinkle id runParDiff emptyModel model panel getBody


main :: IO ()
main = runJSorWarp 8080 app
