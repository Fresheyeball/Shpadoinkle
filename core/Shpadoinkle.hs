{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}



module Shpadoinkle
  ( Html (..), Prop (..)
  , mapHtml,   mapProp
  , Shpadoinkle (..)
  , shpadoinkle
  , RawNode (..)
  ) where


import Prelude hiding ((.))
import           Control.Natural
import Control.Monad.IO.Class
import UnliftIO.Concurrent
import GHC.Conc hiding (forkIO)
import Data.Text
import Data.String
import Data.Kind
import Control.Category
import           GHC.Conc                    (retry)
import           Language.Javascript.JSaddle hiding (( # ))


data Html :: (Type -> Type) -> Type -> Type where
  Node :: Text -> [(Text, Prop m o)] -> [Html m o] -> Html m o
  TextNode :: Text -> Html m o


mapHtml :: (m :~> n) -> Html m o -> Html n o
mapHtml f = \case
  Node t props children -> Node t (fmap (mapProp f) <$> props) (mapHtml f <$> children)
  TextNode t -> TextNode t


mapProp :: (m :~> n) -> Prop m o -> Prop n o
mapProp f = \case
  PListener g -> PListener (f # g)
  PText t     -> PText t
  PFlag       -> PFlag


deriving instance Functor m => Functor (Html m)


data Prop m o = PText Text | PListener (m o) | PFlag


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


newtype RawNode  = RawNode { unRawNode :: JSVal }
instance ToJSVal   RawNode where toJSVal   = return . unRawNode
instance FromJSVal RawNode where fromJSVal = return . Just . RawNode


class Shpadoinkle t m a | t m -> a where
  type VNode t m
  interpret :: (m :~> JSM) -> Html (t m) a -> t m (VNode t m)
  patch :: RawNode -> Maybe (VNode t m) -> VNode t m -> t m (VNode t m)
  setup :: JSM () -> t m ()


shpadoinkle
  :: forall t m a
   . Shpadoinkle t m a
  => Eq a
  => MonadIO (t m)
  => (m :~> JSM)
  -> (t m :~> m)
  -> TVar a
  -> (a -> Html (t m) a)
  -> t m RawNode
  -> JSM ()
shpadoinkle toJSM toM model view stage = do
  let
    j :: t m :~> JSM
    j = toJSM . toM

    go :: RawNode -> VNode t m -> TVar a -> JSM ()
    go c n p = do
      a  <- unwrapNT j . liftIO . atomically $ do
        new' <- readTVar model
        old  <- readTVar p
        if new' == old then retry else new' <$ writeTVar p new'
      m  <- liftJSM (j # interpret toJSM (view a))
      n' <- liftJSM (j # patch c (Just n) m)
      go c n' p

  i' <- liftIO $ readTVarIO model
  p  <- liftIO $ newTVarIO i'
  unwrapNT j . setup $ do
    c <- j # stage
    n <- j # interpret toJSM (view i')
    _ <- forkIO $ go c n p
    _ <- j # patch c Nothing n
    return ()
