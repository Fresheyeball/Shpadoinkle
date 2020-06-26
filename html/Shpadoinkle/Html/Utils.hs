{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Shpadoinkle.Html.Utils where


import           Control.Monad      (forM_)
import           Data.Text
import           GHCJS.DOM
import           GHCJS.DOM.Document as Doc
import           GHCJS.DOM.Element
import           GHCJS.DOM.Node
import           GHCJS.DOM.Types    (MonadJSM, liftJSM, toJSVal, ToJSString)

import           Shpadoinkle


default (Text)


addStyle :: MonadJSM m => Text -> m ()
addStyle x = do
  doc <- currentDocumentUnchecked
  link <- createElement doc "link"
  setAttribute link "href" x
  setAttribute link "rel" "stylesheet"
  headRaw <- Doc.getHeadUnsafe doc
  () <$ appendChild headRaw link


addInlineStyle :: ToJSString css => MonadJSM m => css -> m ()
addInlineStyle bs = do
  doc <- currentDocumentUnchecked
  style <- createElement doc "style"
  setInnerHTML style bs
  headRaw <- Doc.getHeadUnsafe doc
  () <$ appendChild headRaw style


setTitle :: MonadJSM m => Text -> m ()
setTitle t = do
  doc <- currentDocumentUnchecked
  Doc.setTitle doc t


getBody :: MonadJSM m => m RawNode
getBody = do
  doc <- currentDocumentUnchecked
  body <- Doc.getBodyUnsafe doc
  setInnerHTML body ""
  liftJSM $ RawNode <$> toJSVal body


addMeta :: [(Text, Text)] -> JSM ()
addMeta ps = do
  doc <- currentDocumentUnchecked
  tag <- createElement doc ("meta" :: Text)
  forM_ ps $ uncurry (setAttribute tag)
  headRaw <- Doc.getHeadUnsafe doc
  () <$ appendChild headRaw tag


addScriptSrc :: Text -> JSM ()
addScriptSrc src = do
  doc <- currentDocumentUnchecked
  tag <- createElement doc ("script" :: Text)
  setAttribute tag ("src" :: Text) src
  headRaw <- Doc.getHeadUnsafe doc
  () <$ appendChild headRaw tag


addScriptText :: Text -> JSM ()
addScriptText js = do
  doc <- currentDocumentUnchecked
  tag <- createElement doc ("script" :: Text)
  setAttribute tag ("type" :: Text) ("text/javascript" :: Text)
  headRaw <- Doc.getHeadUnsafe doc
  jsn <- createTextNode doc js
  _ <- appendChild tag jsn
  () <$ appendChild headRaw tag
