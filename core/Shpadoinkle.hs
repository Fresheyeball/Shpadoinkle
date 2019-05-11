{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
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
  , type (~>)
  , RawNode (..), RawEvent (..)
  , h, text, flag, listener
  , props, children, name, textContent
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
  PFlag       -> PFlag


h :: Text -> [(Text, Prop m o)] -> [Html m o] -> Html m o
h = Node
text :: Text -> Html m o
text = TextNode
flag :: Prop m o
flag = PFlag
listener :: m o -> Prop m o
listener = PListener . const . const


deriving instance Functor m => Functor (Html m)


data Prop m o = PText Text | PListener (RawNode -> RawEvent -> m o) | PFlag


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


class Shpadoinkle t m a | t m -> a where
  type VNode t m
  interpret :: (m ~> JSM) -> Html (t m) a -> t m (VNode t m)
  patch :: RawNode -> Maybe (VNode t m) -> VNode t m -> t m (VNode t m)
  setup :: JSM () -> t m ()


shpadoinkle
  :: forall t m a
   . Shpadoinkle t m a
  => Eq a
  => MonadIO (t m)
  => (m ~> JSM)
  -> (t m ~> m)
  -> TVar a
  -> (a -> Html (t m) a)
  -> t m RawNode
  -> JSM ()
shpadoinkle toJSM toM model view stage = do
  let
    j :: t m ~> JSM
    j = toJSM . toM

    go :: RawNode -> VNode t m -> TVar a -> JSM ()
    go c n p = do
      a  <- j . liftIO . atomically $ do
        new' <- readTVar model
        old  <- readTVar p
        if new' == old then retry else new' <$ writeTVar p new'
      m  <- j $ interpret toJSM (view a)
      n' <- j $ patch c (Just n) m
      go c n' p

  i' <- liftIO $ readTVarIO model
  p  <- liftIO $ newTVarIO i'
  j . setup $ do
    c <- j stage
    n <- j $ interpret toJSM (view i')
    _ <- forkIO $ go c n p
    _ <- j $ patch c Nothing n :: JSM (VNode t m)
    return ()
