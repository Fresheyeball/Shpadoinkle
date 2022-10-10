{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Shpadoinkle.Html.Utils where


import           Control.Monad     (forM_)
import           Data.Function     ((&))
import           Data.Text         (Text)
import           Shpadoinkle       (MonadJSM, RawNode (RawNode))
import           Shpadoinkle.JSFFI as JSFFI (JSElement, JSString, To,
                                             appendChild, body, createElement,
                                             createTextNode, document,
                                             getElementById, getProp', liftJSM,
                                             setAttribute, setInnerHTML,
                                             setTitle, upcast)


default (Text)


getHead :: MonadJSM m => m JSElement
getHead = getProp' "head" document


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


addInlineStyle :: (To m JSString css, MonadJSM m) => css -> m ()
addInlineStyle bs = do
  style <- createElement "style"
  style & setInnerHTML bs
  getHead >>= appendChild style


setTitle :: MonadJSM m => Text -> m ()
setTitle = JSFFI.setTitle


getBody :: MonadJSM m => m RawNode
getBody = do
  body & setInnerHTML ""
  pure $ RawNode (upcast body)


addMeta :: MonadJSM m => [(Text, Text)] -> m ()
addMeta ps = liftJSM $ do
  tag <- createElement ("meta" :: Text)
  forM_ ps $ (\(k, v) -> setAttribute k v tag)
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
getById eid = (fmap . fmap) (RawNode . upcast) (getElementById eid)


treatEmpty :: Foldable f => Functor f => a -> (f a -> a) -> (b -> a) -> f b -> a
treatEmpty zero plural singular xs = if Prelude.null xs then zero else plural $ singular <$> xs
