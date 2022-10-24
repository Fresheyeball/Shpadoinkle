{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Shpadoinkle.Website.Component.LiveExample where


import           Control.Lens                              ((%~), (&), (.~),
                                                            (^.))
import           Control.Monad.Reader                      (asks)
import           Data.ByteString.Lazy                      as BS (fromStrict)
import           Data.FileEmbed                            (embedFile)
import           Data.Generics.Labels                      ()
import           Data.List                                 as List (intersperse)
import           Data.Text                                 as T
import           Data.Text.Lazy                            as TL (fromStrict,
                                                                  splitOn,
                                                                  toStrict)
import           Data.Text.Lazy.Encoding                   (decodeUtf8,
                                                            encodeUtf8)
import           Prelude                                   hiding (div)
import           Servant.API                               (toUrlPiece)
import           Shpadoinkle                               (Continuation, Html,
                                                            RawNode (..),
                                                            atomically, baked,
                                                            constUpdate, done,
                                                            kleisli, mapC, pur,
                                                            readTVarIO, text,
                                                            writeTVar)
import           Shpadoinkle.Html                          (br'_, class', div,
                                                            div', iframe,
                                                            mkGlobalMailboxAfforded,
                                                            onInput, src)
import           Shpadoinkle.Isreal.Types                  as Swan
import           Shpadoinkle.JSFFI                         (JSM, JSObject,
                                                            JSVal, MonadJSM,
                                                            downcast,
                                                            downcastJSM, global,
                                                            mkEmptyObject,
                                                            mkFun', setProp,
                                                            setTimeout, (#),
                                                            (#-))
import           Shpadoinkle.Lens                          (onRecord)
import           Shpadoinkle.Website.Style
import           Shpadoinkle.Website.Types.Effects.Example (ExampleEffects,
                                                            Swan (..))
import           Shpadoinkle.Website.Types.Example         (Example (..))
import           Shpadoinkle.Website.Types.ExampleState    (ExampleState (..))
import           Shpadoinkle.Website.Types.Home            (ExampleLens)


default (Text)


example :: (MonadJSM m, ExampleEffects m) => ExampleLens -> Example -> Html m Example
example lmutex (Example cc token nonce state') = div [ class' minisandbox ]
  [ div [ class' minisandbox__code
        , onInput . const $ #state .~ ELoading
        ]
    [ div [ class' [ minisandbox__header, minisandbox__code__header ] ]
      [ "Live Shpadoinkle Editor" ]
    , div [ class' [ minisandbox__code__content, "mirror-wrap" ] ]
      [ mapC (mappend (compileExample lmutex) . onRecord #inputHaskell) $ mirror cc ]
    ]
  , div [ class' minisandbox__output ]
    [ div [ class' [ minisandbox__header, minisandbox__output__header ] ]
      [ "Result" ]
    , div [ class' minisandbox__output__content ]
      [ case state' of
          EReady -> iframe
            [ src $ "https://isreal.shpadoinkle.org/"
                 <> toUrlPiece (Swan.serve token)
                 <> "/index.html?nonce="
                 <> pack (show $ nonce - 1)
            ] []
          EError e -> errorMessages e
          ELoading -> div' [ class' lds_dual_ring ]
      ]
    ]
  ]


exampleTemplate :: Code -> Code
exampleTemplate (Code inputHaskell') = Code
   $ BS.fromStrict $(embedFile "./example.template.top")
  <> inputHaskell'
  <> BS.fromStrict $(embedFile "./example.template.bottom")


topOffset :: Int
topOffset = subtract 1
  . Prelude.length
  . TL.splitOn "\n"
  . decodeUtf8
  $ BS.fromStrict $(embedFile "./example.template.top")


compileExample :: (MonadJSM m, ExampleEffects m) => ExampleLens -> Continuation m Example
compileExample lmutex = kleisli $ \(Example cc token nonce _) -> do
  mutex <- asks (^. lmutex)
  cur <- readTVarIO mutex
  atomically $ writeTVar mutex $ Just cc
  case cur of
    Just _ -> return done
    Nothing -> do
      res <- compile token nonce $ exampleTemplate cc
      cur' <- readTVarIO mutex
      case cur' of
        Nothing -> ret res 1
        Just cc' | cc' == cc -> do
          atomically $ writeTVar mutex Nothing
          ret res 1
        Just cc' -> do
          atomically $ writeTVar mutex Nothing
          res' <- compile token (nonce + 1) $ exampleTemplate cc'
          ret res' 2
  where
  ret res (n :: SnowNonce) = return . pur $ (#snowNonce %~ (+ n)) . case res of
    Left e  -> #state .~ EError e
    Right _ -> #state .~ EReady


errorMessages :: CompileError -> Html m a
errorMessages = div "errors" . fmap singleError . breakError . replace bunkWarning "" . unCompileError
  where
  singleError e =
    let offLineNumber = Prelude.head . T.splitOn ":" $ T.drop 1 e
        lineNumber = pack . show . subtract topOffset . read $ unpack offLineNumber
        pad = let padSize = T.length offLineNumber - T.length lineNumber in if padSize > 0 then T.replicate padSize " " else ""
        replaceLineNumber = replace (offLineNumber <> ":") (lineNumber <> ":") . replace (offLineNumber <> " |") (pad <> lineNumber <> " |")
    in div "error" . List.intersperse br'_ . fmap (text . replaceLineNumber) . T.splitOn "\n" $ T.drop 1 e
  breakError = Prelude.filter (not . T.null . strip) . T.splitOn "Main.hs"
  bunkWarning = "Warning: don't know how to find change monitoring files for the installed\npackage databases for ghcjs\n"


mirrorCfg :: Code -> JSM JSObject
mirrorCfg (Code cc) = do
  cfg <- mkEmptyObject
  cfg & setProp "mode" "haskell"
  cfg & setProp "theme" "darcula"
  cfg & setProp "value" (toStrict $ decodeUtf8 cc)
  cfg & setProp "indentUnit" (4 :: Int)
  cfg & setProp "matchBrackets" True
  cfg & setProp "lineNumbers" True
  return cfg


mirror :: Code -> Html m Code
mirror cc = baked $ do
  (notify, stream) <- mkGlobalMailboxAfforded constUpdate
  container' :: JSVal <- global # "createElement" $ "div"
  cfg <- mirrorCfg cc
  cm :: JSVal  <- global # "CodeMirror" $ (container', cfg)
  cmo :: JSObject <- downcastJSM cm
  onChange <- mkFun' $ \_ -> do
        jsv :: JSVal <- cmo # "getValue" $ ()
        let raw :: Maybe Text = downcast jsv
        maybe (pure ()) (notify . Code . encodeUtf8 . TL.fromStrict) raw
  cmo #- "on" $ ("change", onChange)
  _ <- setTimeout 33 =<< (mkFun' $ \_ -> cmo #- "refresh" $ ())
  container'' <- downcastJSM container'
  return (RawNode container'', stream)
