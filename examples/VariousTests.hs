{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults       #-}

module Main where

import           Data.Function                 ((&))
import           Data.Maybe                    (fromMaybe)
import           Data.Semigroup                (stimes)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           GHC.Generics                  (Generic)
import           Network.HTTP.Types.Version    (http20)
import           Servant.Client.Core.Request   (RequestF (..))
import           Servant.Client.Core.Response  (ResponseF (..))
import           Servant.Client.JS             (runClientM,
                                                withStreamingRequestJSM)
import           Servant.Types.SourceT         (foreach)
import           Shpadoinkle                   (Html, JSM, NFData)
import           Shpadoinkle.Backend.ParDiff   (runParDiff)
import           Shpadoinkle.Console           (ToJSON, ToJSVal, askJSM, logJS,
                                                trapper)
import qualified Shpadoinkle.Html              as H
import           Shpadoinkle.Html.LocalStorage (getStorage, setStorage)
import           Shpadoinkle.JSFFI             (MonadJSM, console, liftJSM,
                                                (#-))
import           Shpadoinkle.Router.Client     (BaseUrl (..), ClientEnv (..),
                                                Scheme (Http), getClientEnv)
import           Shpadoinkle.Run               (run, simple)


newtype Model = Model { val :: Int }
  deriving (Show, Generic, NFData, Eq)

initial :: Model
initial = Model { val = 0 }


view :: MonadJSM m => Model -> Html m Model
view m = H.div "calculator"
  [ H.div [] [ H.text "hello, there!" ]
  , H.div []
    ([ H.input
      [ H.type' "text"
      , H.onClickAwayM             (        logIt "[input] click away")
      , H.onKeydownM               (const $ logIt "[input] key down")
      , H.onGlobalKeyDownM         (const $ logIt "[input] global key down")
      -- , H.onGlobalKeyDownNoRepeatM (const $ logIt "[input] global key down (no repeat)")
      -- nb. Both onGlobalKeyDown and onGlobalKeyDownNoRepeat seem to work, but not together
      , H.onEscapeM                (        logIt "[input] escape")
      ]
      []
    , H.text " "
    ] & stimes 2)
  , H.div []
    [ H.button
      [ H.onClickM $ do
            mx <- getStorage "val"
            let x = fromMaybe 0 mx
            let x' = x + 1
            setStorage "val" x'
            pure (\m' -> m' { val = x' })
      ]
      [ H.text "inc" ]
    , H.text " "
    , H.text . T.pack . show $ val m
    ]
  ]

  where

  logIt str = liftJSM $ id <$ (console #- "log" $ [str])


app :: JSM ()
app = do
  H.setTitle "Calculator"
  ctx <- askJSM

  logJS @Show "info" ["hello", "there"]
  logJS @Show "warn" ["hello", "here"]
  logJS @ToJSON "info" ["hello", "there"]
  logJS @ToJSON "warn" ["hello", "here"]
  logJS @ToJSVal "info" ("hello / there" :: Text)
  logJS @ToJSVal "warn" ("hello / here" :: Text)

  clientEnv@ClientEnv { baseUrl } <- getClientEnv
  console #- "log" $ [ show baseUrl ]

  do
    -- Test withStreamingRequestJSM
    let fakeEnv = clientEnv { baseUrl = BaseUrl Http "google.com" 80 "" }
    (>>= (\e -> console #- "log" $ [either (\_ -> "<err>") (\() -> "<ok>") e])) $
      flip runClientM fakeEnv $
        withStreamingRequestJSM
          (Request
              { requestPath = mempty
              , requestQueryString = mempty
              , requestBody = Nothing
              , requestAccept = mempty
              , requestHeaders = mempty
              , requestHttpVersion = http20
              , requestMethod = "GET"
              })
          (\(Response _ _ _ body) ->
              body & foreach
                        (\err -> console #- "log" $ [err])
                        (\byteStr -> console #- "log" $ [show byteStr])
          )

  simple runParDiff initial (view . trapper @Show ctx) H.getBody


main :: IO ()
main = run app
