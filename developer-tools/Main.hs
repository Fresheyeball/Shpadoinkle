{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where


import           Control.Lens                hiding ((#))
import           Control.Monad               (void, when)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Map                    as Map (Map, insert, lookup,
                                                     toDescList)
import           Data.Text                   (Text, pack, unpack)
import           Data.Time                   (UTCTime, defaultTimeLocale,
                                              formatTime, getCurrentTime)
import           GHC.Generics                (Generic)
import           Prelude                     hiding (div, span)
import           Shpadoinkle.JSFFI           (JSKey, JSM, JSObject, MonadJSM,
                                              To, fromJSValUnsafe, getProp,
                                              global, jsTreq, jsValToMaybeText,
                                              liftJSM, mkEmptyObject, mkFun',
                                              purely, setProp, toJSVal, (#))
import qualified Text.Show.Pretty            as Pretty
import           UnliftIO                    (TVar, atomically, modifyTVar,
                                              newTVarIO)

import           Shpadoinkle                 (Html, NFData, flagProp,
                                              shpadoinkle, text)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html            hiding (onMessage)
import           Shpadoinkle.Run             (run)


default (Text)


newtype History = History { unHistory :: Text }
  deriving (Eq, Ord, Show, Generic, NFData)


data Model = Model
  { _history :: Map UTCTime History
  , _active  :: Maybe UTCTime
  , _sync    :: Bool
  } deriving (Eq, Show, Generic, NFData)
makeLenses ''Model


emptyModel :: Model
emptyModel = Model mempty Nothing True


(!) :: (MonadJSM m, To m JSKey key, To m JSObject obj) => m obj -> key -> m JSObject
o ! k = fromJSValUnsafe @JSObject <$> (getProp k =<< o)


listenForOutput :: TVar Model -> JSM ()
listenForOutput model = do
  onMessage <- pure global ! "chrome" ! "runtime" ! "onMessage"
  void $ (onMessage # "addListener") =<< mkFun' (\args -> do
    let x = fromJSValUnsafe @JSObject $ Prelude.head args
    t <- getProp "type" x
    let isRight = t `jsTreq` purely toJSVal "shpadoinkle_output_state"
    when isRight $ do
      msg <- getProp "msg" x
      now <- liftIO getCurrentTime
      let history' = maybe (error "how could this not be a string") History $ jsValToMaybeText msg
      atomically . modifyTVar model $ heard now history')


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
  tabId <- (pure global ! "chrome" ! "devtools" ! "inspectedWindow") >>= getProp "tabId"

  msg <- mkEmptyObject
  msg & setProp "type" (purely toJSVal "shpadoinkle_set_state")
  msg & setProp "msg" history'

  void $ (pure global ! "chrome" ! "tabs") >>= (\t -> t # "sendMessage" $ (tabId, msg))


prettyHtml :: Monad m => Int -> Pretty.Value -> Html m a
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
  Pretty.InfixCons _ _ -> text "Infix Constructors are not currently supported"
  Pretty.Neg x     -> div "neg" [ "¬", prettyHtml depth x ]
  Pretty.Ratio n d -> div "ratio" [ prettyHtml depth n, "/", prettyHtml depth d ]
  Pretty.Tuple xs  -> prettyHtml depth $ Pretty.Con "(,)" xs
  Pretty.List []   -> prettyHtml depth $ Pretty.Con "[]" []
  Pretty.List xs   -> ul "list" $ li_.pure.prettyHtml (depth +1) <$> xs
  Pretty.String ss -> div "string" $ string ss
  Pretty.Float n   -> div "float" $ string n
  Pretty.Integer n -> div "integer" $ string n
  Pretty.Char c    -> div "char" $ string c
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
  shpadoinkle id runParDiff model panel getBody


main :: IO ()
main = run app
