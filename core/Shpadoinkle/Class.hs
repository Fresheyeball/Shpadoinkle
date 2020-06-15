{-# LANGUAGE ExplicitNamespaces     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Shpadoinkle.Class
  ( RawNode (..)
  , RawEvent (..)
  , IsHtml (..)
  , IsProp (..)
  , type (~>)
  , listen, listen'
  , listener, listenRaw
  , mapProps, mapChildren, injectProps
  ) where


import           Data.Functor.Identity       (Identity (..))
import           Data.String
import           Data.Text
import           Language.Javascript.JSaddle (FromJSVal (..), JSM, JSVal,
                                              ToJSVal (..))

-- | Natural Transformation
type m ~> n = forall a. m a -> n a


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
class IsHtml h p | h -> p where

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


-- | Abstraction of property types subsuming `Prop m` and `Prop'`.
class IsProp p e | p -> e where

  -- | Create a text property.
  textProp :: Text -> p a

  -- | Create an event listener property.
  listenerProp :: (RawNode -> RawEvent -> JSM (e a)) -> p a

  -- | Create a boolean property.
  flagProp :: Bool -> p a


-- | Strings are overloaded as the class property:
-- @
--   "active" = ("className", PText "active")
-- @
instance {-# OVERLAPPING #-} IsProp p e => IsString [(Text, p a)] where
  fromString = pure . ("className", ) . textProp . pack
  {-# INLINE fromString #-}


-- | Construct a simple listener property that will perform an action.
listener :: IsProp p e => e a -> p a
listener = listenerProp . const . const . return
{-# INLINE listener #-}


-- | Construct a listener from its name and an event handler.
listenRaw :: IsProp p e => Text -> (RawNode -> RawEvent -> JSM (e a)) -> (Text, p a)
listenRaw k = (,) k . listenerProp
{-# INLINE listenRaw #-}


-- | Construct a listener from its name and an event handler.
listen :: IsProp p e => Text -> e o -> (Text, p o)
listen k = listenRaw k . const . const . return
{-# INLINE listen #-}


-- | Construct a listener from it's 'Text' name and an output value.
listen' :: IsProp p Identity => Text -> o -> (Text, p o)
listen' k f = listen k $ pure f
{-# INLINE listen' #-}


-- | Transform the properties of some Node. This has no effect on @TextNode@s or @Potato@s
mapProps :: IsHtml h p => ([(Text, p o)] -> [(Text, p o)]) -> h o -> h o
mapProps f = runIdentity . props (Identity . f)
{-# INLINE mapProps #-}


-- | Transform the children of some Node. This has no effect on @TextNode@s or @Potato@s
mapChildren :: IsHtml h p => ([h a] -> [h a]) -> h a -> h a
mapChildren f = runIdentity . children (Identity . f)
{-# INLINE mapChildren #-}


-- | Inject props into an existing @Node@
injectProps :: IsHtml h p => [(Text, p o)] -> h o -> h o
injectProps ps = mapProps (++ ps)
{-# INLINE injectProps #-}
