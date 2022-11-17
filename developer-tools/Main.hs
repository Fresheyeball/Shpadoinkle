{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
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
import           Shpadoinkle.JSFFI           (JSKey, JSM, JSObject, JSVal,
                                              MonadJSM, getProp, getPropMaybe,
                                              global, jsAs, jsTo, liftJSM,
                                              mkEmptyObject, mkFun', setProp,
                                              type (<:), (#-), (===))
import qualified Text.Show.Pretty            as Pretty
import           UnliftIO                    (TVar, atomically, modifyTVar,
                                              newTVarIO)

import           Shpadoinkle                 (Html, NFData, flagProp,
                                              shpadoinkle, text)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import qualified Shpadoinkle.Html            as H
import           Shpadoinkle.JSFFI           (body)
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


(!) :: (MonadJSM m, key <: JSKey, obj <: JSObject) => m obj -> key -> m JSObject
o ! k = getProp (jsAs @JSKey k) =<< o


listenForOutput :: TVar Model -> JSM ()
listenForOutput model = do
  onMessage <- pure global ! "chrome" ! "runtime" ! "onMessage"
  (onMessage #- "addListener") =<< mkFun' (\args -> do
    x <- jsTo @JSObject $ Prelude.head args
    t :: Text <- getProp "type" x
    let isRight = t === "shpadoinkle_output_state"
    when isRight $ do
      msg :: Maybe Text <- getPropMaybe "msg" x
      now <- liftIO getCurrentTime
      let history' = maybe (error "how could this not be a string") History msg
      atomically . modifyTVar model $ heard now history')


heard :: UTCTime -> History -> Model -> Model
heard now history' m = m & history %~ insert now history' &
  case m ^. active of
    Just _ | m ^. sync -> active ?~ now
    Nothing            -> active ?~ now
    Just _             -> id


row :: MonadJSM m => Maybe UTCTime -> UTCTime -> History -> Html m Model
row sel k history' = H.div "record"
  [ H.div [ H.className "time"
          , H.class' [("active", sel == Just k)]
          ]
     [ H.span_ [ H.text . pack $ formatTime defaultTimeLocale "%X%Q" k ]
     , H.button [ H.onClick $ (sync .~ False) . (active ?~ k) ] [ "Inspect" ]
     , H.button [ H.onClickM_ . liftJSM $ sendHistory history' ] [ "Send" ]
     ]
  ]


sendHistory :: History -> JSM ()
sendHistory (History history') = void $ do
  tabId <- (pure global ! "chrome" ! "devtools" ! "inspectedWindow") >>= getProp @JSVal "tabId"

  msg <- mkEmptyObject
  msg & setProp "type" "shpadoinkle_set_state"
  msg & setProp "msg" history'

  (pure global ! "chrome" ! "tabs") >>= (\t -> t #- "sendMessage" $ (tabId, msg))


prettyHtml :: Monad m => Int -> Pretty.Value -> Html m a
prettyHtml depth = \case
  Pretty.Con con [] -> H.div "con-uniary" $ string con
  Pretty.Con con slots -> H.details [ H.className "con-wrap", ("open", flagProp $ depth < 3) ]
    [ H.summary "con" $ string con
    , H.div (withDepth "con-children") $ prettyHtml (depth + 1) <$> slots
    ]
  Pretty.Rec rec fields ->
    H.details (withDepth "rec-wrap")
    [ H.summary "rec" $ string rec
    , H.dl "rec" $ (\(n, v)->
        [ H.dt_ $ string $ n <> " = "
        , H.dd_ [ prettyHtml (depth + 1) v ]
        ]) =<< fields
    ]
  Pretty.InfixCons _ _ -> H.text "Infix Constructors are not currently supported"
  Pretty.Neg x     -> H.div "neg" [ "¬", prettyHtml depth x ]
  Pretty.Ratio n d -> H.div "ratio" [ prettyHtml depth n, "/", prettyHtml depth d ]
  Pretty.Tuple xs  -> prettyHtml depth $ Pretty.Con "(,)" xs
  Pretty.List []   -> prettyHtml depth $ Pretty.Con "[]" []
  Pretty.List xs   -> H.ul "list" $ H.li_.pure.prettyHtml (depth +1) <$> xs
  Pretty.String ss -> H.div "string" $ string ss
  Pretty.Float n   -> H.div "float" $ string n
  Pretty.Integer n -> H.div "integer" $ string n
  Pretty.Char c    -> H.div "char" $ string c
  where string = pure . text . pack
        withDepth x = [ H.class' [ x, "depth-" <> pack (show depth) ] ]


syncState :: Model -> Model
syncState m =
  m & sync .~ True & active .~ (m ^. history . to (g . toDescList))
  where g ((x,_):_) = Just x
        g _         = Nothing


panel :: MonadJSM m => Model -> Html m Model
panel m = H.div "wrapper"
  [ H.div "current-state" $ case _active m >>= flip Map.lookup (_history m) of
      Just history' -> [ maybe (H.text "failed to parse value") (prettyHtml 0) . Pretty.parseValue . unpack $ unHistory history' ]
      _             -> [ "No State" ]
  , H.div "history" $ H.button
    [ H.onClick syncState
    , H.className "sync-button"
    , H.class' [ ("sync", m ^. sync) ]
    ] [ "Sync State" ] : (book <> [clear])
  ]
  where book = uncurry (m ^. active . to row) <$> Map.toDescList (m ^. history)
        clear = H.a [ H.className "clear", H.onClick $ const emptyModel ] [ "Clear History" ]


app :: JSM ()
app = do
  model <- liftIO $ newTVarIO emptyModel
  listenForOutput model
  shpadoinkle id runParDiff model panel H.getBody


main :: IO ()
main = run app
