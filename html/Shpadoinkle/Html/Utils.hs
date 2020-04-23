{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Shpadoinkle.Html.Utils where


import           Data.Text
import           GHCJS.DOM
import           GHCJS.DOM.Document as Doc
import           GHCJS.DOM.Element
import           GHCJS.DOM.Node
import           GHCJS.DOM.Types    (toJSVal)

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
