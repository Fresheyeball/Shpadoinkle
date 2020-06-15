{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE UndecidableInstances   #-}


module Shpadoinkle.Class
  ( RawNode (..)
  , RawEvent (..)
  , HtmlClass (..)
  ) where


import           Data.Text
import           Language.Javascript.JSaddle (FromJSVal (..), JSM, JSVal,
                                              ToJSVal (..))


-- | A DOM node reference.
-- Useful for building baked potatoes, and binding a Backend view to the page
newtype RawNode  = RawNode  { unRawNode  :: JSVal }
instance ToJSVal   RawNode where toJSVal   = return . unRawNode
instance FromJSVal RawNode where fromJSVal = return . Just . RawNode


-- | A raw event object reference
newtype RawEvent = RawEvent { unRawEvent :: JSVal }
instance ToJSVal   RawEvent where toJSVal   = return . unRawEvent
instance FromJSVal RawEvent where fromJSVal = return . Just . RawEvent


-- | Abstraction of HTML types subsuming `Html m` and `Html'`.
class HtmlClass h p | h -> p where

  -- | Construct an HTML element JSX-style.
  h :: Text -> [(Text, p o)] -> [h o] -> h o

  -- | Construct a 'Potato' from a 'JSM' action producing a 'RawNode'.
  baked :: JSM RawNode -> h o

  -- | Construct a text node.
  text :: Text -> h o

  -- | Lens to props
  props :: Applicative f => ([(Text, p o)] -> f [(Text, p o)]) -> h o -> f (h o)

  -- | Lens to children
  children :: Applicative f => ([h o] -> f [h o]) -> h o -> f (h o)

  -- | Lens to tag name
  name :: Applicative f => (Text -> f Text) -> h o -> f (h o)

  -- | Lens to content of @TextNode@s
  textContent :: Applicative f => (Text -> f Text) -> h o -> f (h o)

  -- | Construct an HTML element out of heterogeneous alternatives.
  eitherH :: (a -> h a) -> (b -> h b) -> Either a b -> h (Either a b)
