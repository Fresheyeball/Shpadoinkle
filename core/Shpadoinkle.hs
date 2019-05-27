{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE ExplicitNamespaces     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}


module Shpadoinkle
  ( Html (..), Prop (..)
  , mapHtml, mapProp, mapProps, mapChildren
  , Shpadoinkle (..)
  , shpadoinkle
  , Territory (..)
  , type (~>)
  , RawNode (..), RawEvent (..)
  , h, text, flag, listener
  , props, children, name, textContent
  , MonadJSM, JSM
  , newTVarIO
  ) where


import           Control.Monad.IO.Class
import           Data.Kind
import           Data.String
import           Data.Text
import           GHC.Conc                    hiding (forkIO)
import           GHC.Conc                    (retry)
import           Language.Javascript.JSaddle
import           UnliftIO.Concurrent


data Html :: (Type -> Type) -> Type -> Type where
  Node :: Text -> [(Text, Prop m o)] -> [Html m o] -> Html m o
  TextNode :: Text -> Html m o


type m ~> n = forall a. m a -> n a


mapHtml :: (m ~> n) -> Html m o -> Html n o
mapHtml f = \case
  Node t ps cs -> Node t (fmap (mapProp f) <$> ps) (mapHtml f <$> cs)
  TextNode t -> TextNode t


mapProps :: ([(Text, Prop m o)] -> [(Text, Prop m o)]) -> Html m o -> Html m o
mapProps f = \case
  Node t ps cs -> Node t (f ps) cs
  t -> t


mapChildren :: ([Html m a] -> [Html m a]) -> Html m a -> Html m a
mapChildren f = \case
  Node t ps cs -> Node t ps (f cs)
  t -> t


props :: Applicative f => ([(Text, Prop m a)] -> f [(Text, Prop m a)]) -> Html m a -> f (Html m a)
props inj = \case
  Node t ps cs -> (\ps' -> Node t ps' cs) <$> inj ps
  t -> pure t


children :: Applicative f => ([Html m a] -> f [Html m a]) -> Html m a -> f (Html m a)
children inj = \case
  Node t ps cs -> Node t ps <$> inj cs
  t -> pure t


name :: Applicative f => (Text -> f Text) -> Html m a -> f (Html m a)
name inj = \case
  Node t ps cs -> (\t' -> Node t' ps cs) <$> inj t
  t -> pure t


textContent :: Applicative f => (Text -> f Text) -> Html m a -> f (Html m a)
textContent inj = \case
  TextNode t -> TextNode <$> inj t
  n -> pure n


mapProp :: (m ~> n) -> Prop m o -> Prop n o
mapProp f = \case
  PListener g -> PListener (\x y -> f (g x y))
  PText t     -> PText t
  PFlag b     -> PFlag b


h :: Text -> [(Text, Prop m o)] -> [Html m o] -> Html m o
h = Node
text :: Text -> Html m o
text = TextNode
flag :: Bool -> Prop m o
flag = PFlag
listener :: m o -> Prop m o
listener = PListener . const . const


deriving instance Functor m => Functor (Html m)


data Prop m o = PText Text | PListener (RawNode -> RawEvent -> m o) | PFlag Bool


deriving instance Functor m => Functor (Prop m)


instance IsString (Html m o) where
  fromString = TextNode . pack
  {-# INLINE fromString #-}


instance IsString (Prop m o) where
  fromString = PText . pack
  {-# INLINE fromString #-}


instance {-# OVERLAPPING #-} IsString [(Text, Prop m o)] where
  fromString = pure . ("className", ) . PText . pack
  {-# INLINE fromString #-}


newtype RawNode  = RawNode  { unRawNode  :: JSVal }
newtype RawEvent = RawEvent { unRawEvent :: JSVal }
instance ToJSVal   RawNode where toJSVal   = return . unRawNode
instance FromJSVal RawNode where fromJSVal = return . Just . RawNode


class Shpadoinkle b m a | b m -> a where
  type VNode b m
  interpret :: (m ~> JSM) -> Html (b m) a -> b m (VNode b m)
  patch :: RawNode -> Maybe (VNode b m) -> VNode b m -> b m (VNode b m)
  setup :: JSM () -> b m ()


class Territory s where
  writeUpdate :: s a -> a -> JSM ()
  shouldUpdate :: Eq a => (b -> a -> JSM b) -> b -> s a -> JSM ()


instance Territory TVar where
  writeUpdate x = liftIO . atomically . writeTVar x
  shouldUpdate potato prev model = do
    i' <- liftIO $ readTVarIO model
    p  <- liftIO $ newTVarIO i'
    () <$ forkIO (go prev p)
    where
      go x p = do
        a <- liftIO . atomically $ do
          new' <- readTVar model
          old  <- readTVar p
          if new' == old then retry else new' <$ writeTVar p new'
        y <- potato x a
        go y p


shpadoinkle
  :: forall b m a t
  -- ^ b for backend, m for monad, a for model, t for territory
   . Shpadoinkle b m a
  => Territory t
  => Eq a
  => (m ~> JSM)
  -- ^ how to be get to JSM?
  -> (t a -> b m ~> m)
  -- ^ how do we run the backend?
  -> a
  -- ^ what is the initial state?
  -> t a
  -- ^ how can we know when to update?
  -> (a -> Html (b m) a)
  -- ^ how should the html look?
  -> b m RawNode
  -- ^ where do we render?
  -> JSM ()
shpadoinkle toJSM toM initial model view stage = do
  let
    j :: b m ~> JSM
    j = toJSM . toM model

    go :: RawNode -> VNode b m -> a -> JSM (VNode b m)
    go c n a = do
      !m  <- j $ interpret toJSM (view a)
      j $ patch c (Just n) m

  j . setup $ do
    c <- j stage
    n <- j $ interpret toJSM (view initial)
    _ <- shouldUpdate (go c) n model
    _ <- j $ patch c Nothing n :: JSM (VNode b m)
    return ()
