{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Shpadoinkle.Html.Utils where


import           Control.Monad                  (forM_)
import           Data.Text                      (Text)
import           GHCJS.DOM                      (currentDocumentUnchecked)
import           GHCJS.DOM.Document             as Doc (createElement,
                                                        createTextNode,
                                                        getBodyUnsafe,
                                                        getHeadUnsafe, setTitle)
import           GHCJS.DOM.Element              (setAttribute, setInnerHTML)
import           GHCJS.DOM.Node                 (appendChild)
import           GHCJS.DOM.NonElementParentNode (getElementById)
import           GHCJS.DOM.Types                (ToJSString, liftJSM, toJSVal)

import           Shpadoinkle                    (MonadJSM, RawNode (RawNode))


default (Text)


-- | Add a stylesheet to the page via @link@ tag.
addStyle
  :: MonadJSM m
  => Text
  -- ^ The URI for the @href@ attribute
  -> m ()
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


addMeta :: MonadJSM m => [(Text, Text)] -> m ()
addMeta ps = liftJSM $ do
  doc <- currentDocumentUnchecked
  tag <- createElement doc ("meta" :: Text)
  forM_ ps $ uncurry (setAttribute tag)
  headRaw <- Doc.getHeadUnsafe doc
  () <$ appendChild headRaw tag


createDivWithId :: MonadJSM m => Text -> m ()
createDivWithId did = liftJSM $ do
  doc <- currentDocumentUnchecked
  tag <- createElement doc ("div" :: Text)
  setAttribute tag "id" did
  body <- Doc.getHeadUnsafe doc
  () <$ appendChild body tag


addScriptSrc :: MonadJSM m => Text -> m ()
addScriptSrc src = liftJSM $ do
  doc <- currentDocumentUnchecked
  tag <- createElement doc ("script" :: Text)
  setAttribute tag ("src" :: Text) src
  headRaw <- Doc.getHeadUnsafe doc
  () <$ appendChild headRaw tag


addScriptText :: MonadJSM m => Text -> m ()
addScriptText js = liftJSM $ do
  doc <- currentDocumentUnchecked
  tag <- createElement doc ("script" :: Text)
  setAttribute tag ("type" :: Text) ("text/javascript" :: Text)
  headRaw <- Doc.getHeadUnsafe doc
  jsn <- createTextNode doc js
  _ <- appendChild tag jsn
  () <$ appendChild headRaw tag


getById :: MonadJSM m => Text -> m RawNode
getById did = liftJSM $ do
  doc <- currentDocumentUnchecked
  fmap RawNode . toJSVal =<< getElementById doc did


treatEmpty :: Foldable f => Functor f => a -> (f a -> a) -> (b -> a) -> f b -> a
treatEmpty zero plural singular xs = if Prelude.null xs then zero else plural $ singular <$> xs


