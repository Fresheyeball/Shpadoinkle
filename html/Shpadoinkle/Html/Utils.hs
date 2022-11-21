{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Shpadoinkle.Html.Utils where


import           Control.Monad     (forM_)
import           Data.Function     ((&))
import           Data.Text         (Text)
import           Shpadoinkle       (MonadJSM, RawNode (RawNode))
import           Shpadoinkle.JSFFI as JSFFI (JSHTMLElement, JSString,
                                             appendChild, body, createElement,
                                             createTextNode, document,
                                             getElementById, getProp, jsAs,
                                             liftJSM, setAttribute,
                                             setInnerHTML, setProp, type (<:))


default (Text)


getHead :: MonadJSM m => m JSHTMLElement
getHead = getProp "head" document


-- | Add a stylesheet to the page via @link@ tag.
addStyle
  :: MonadJSM m
  => Text
  -- ^ The URI for the @href@ attribute
  -> m ()
addStyle x = do
  link <- createElement "link"
  link & setAttribute "href" x
  link & setAttribute "rel" "stylesheet"
  getHead >>= appendChild link


addInlineStyle :: (css <: JSString, MonadJSM m) => css -> m ()
addInlineStyle bs = do
  style <- createElement "style"
  style & setInnerHTML bs
  getHead >>= appendChild style


setTitle :: MonadJSM m => Text -> m ()
setTitle title =
  setProp "title" title document


getBody :: MonadJSM m => m RawNode
getBody = do
  body & setInnerHTML ""
  pure $ RawNode (jsAs body)


addMeta :: MonadJSM m => [(Text, Text)] -> m ()
addMeta ps = liftJSM $ do
  tag <- createElement ("meta" :: Text)
  forM_ ps (\(k, v) -> setAttribute k v tag)
  getHead >>= appendChild tag


createDivWithId :: MonadJSM m => Text -> m ()
createDivWithId did = liftJSM $ do
  tag <- createElement ("div" :: Text)
  tag & setAttribute "id" did
  body & appendChild tag


addScriptSrc :: MonadJSM m => Text -> m ()
addScriptSrc src = liftJSM $ do
  tag <- createElement ("script" :: Text)
  tag & setAttribute ("src" :: Text) src
  getHead >>= appendChild tag


addScriptText :: MonadJSM m => Text -> m ()
addScriptText js = liftJSM $ do
  tag <- createElement ("script" :: Text)
  tag & setAttribute ("type" :: Text) ("text/javascript" :: Text)
  jsn <- createTextNode js
  tag & appendChild jsn
  getHead >>= appendChild tag


getById :: MonadJSM m => Text -> m (Maybe RawNode)
getById eid = (fmap . fmap) (RawNode . jsAs) (getElementById eid)


treatEmpty :: Foldable f => Functor f => a -> (f a -> a) -> (b -> a) -> f b -> a
treatEmpty zero plural singular xs = if Prelude.null xs then zero else plural $ singular <$> xs
