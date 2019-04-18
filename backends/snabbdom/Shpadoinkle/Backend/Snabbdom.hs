{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Shpadoinkle.Backend.Snabbdom
  ( SnabbdomT (..)
  , runSnabbdomNT
  , stage
  ) where

import           Control.Category
import           Control.Monad.Reader
import           Control.Natural
import           Data.FileEmbed
import           Data.Text
import           Data.Traversable
import           GHC.Conc
import           Language.Javascript.JSaddle hiding (( # ))
import           Prelude                     hiding ((.))

import           Shpadoinkle                 hiding (children, name, props)


default (Text)


newtype SnabVNode = SnabVNode { unVNode :: JSVal }
instance ToJSVal   SnabVNode where toJSVal   = return . unVNode
instance FromJSVal SnabVNode where fromJSVal = return . Just . SnabVNode


newtype SnabbdomT model m a = Snabbdom { unSnabbdom :: ReaderT (TVar model) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar model), MonadTrans)


deriving instance MonadJSM m => MonadJSM (SnabbdomT model m)


runSnabbdomT :: TVar model -> SnabbdomT model m a -> m a
runSnabbdomT t (Snabbdom r) = runReaderT r t


runSnabbdomNT :: TVar model -> SnabbdomT model m :~> m
runSnabbdomNT t = NT $ runSnabbdomT t


props :: (m :~> JSM) -> TVar a -> [(Text, Prop (SnabbdomT a m) a)] -> JSM Object
props toJSM i xs = do
  o <- create
  a <- create
  e <- create
  void $ xs `for` \(k, p) -> case p of
    PText t -> do
      t' <- toJSVal t
      unsafeSetProp (JSString k) t' a
    PListener f -> do
      f' <- toJSVal . fun $ \_ _ _ ->
        liftIO . atomically . writeTVar i =<< unwrapNT (toJSM . NT (runSnabbdomT i)) f
      unsafeSetProp (JSString k) f' e
    PFlag -> do
      f <- toJSVal True
      unsafeSetProp (JSString k) f a

  p <- toJSVal a
  l <- toJSVal e
  unsafeSetProp "props" p o
  unsafeSetProp "on" l o
  return o


instance (MonadJSM m, Eq a) => Shpadoinkle (SnabbdomT a) m a where
  type VNode (SnabbdomT a) m = SnabVNode

  interpret :: (m :~> JSM) -> Html (SnabbdomT a m) a -> SnabbdomT a m SnabVNode
  interpret toJSM = \case

    TextNode t -> liftJSM $ fromJSValUnchecked =<< toJSVal t

    Node name ps [TextNode t] -> do
      i <- ask; liftJSM $ do
        o <- props toJSM i ps
        fromJSValUnchecked =<< jsg3 "vnode" name o t

    Node name ps children -> do
      i <- ask; liftJSM $ do
        o <- props toJSM i ps
        traverse (unwrapNT (toJSM . NT (runSnabbdomT i)) . interpret toJSM >=> toJSVal) children
          >>= jsg3 "vnode" name o >>= fromJSValUnchecked


  patch :: RawNode -> Maybe SnabVNode -> SnabVNode -> SnabbdomT a m SnabVNode
  patch (RawNode r) f t = t <$ (liftJSM . void $ jsg2 "patch" f' t)
    where f' = maybe r unVNode f


  setup :: JSM () -> SnabbdomT a m ()
  setup cb = liftJSM $ do
    void $ eval @Text $(embedStringFile "Shpadoinkle/Backend/Snabbdom/Setup.js")
    void . jsg1 "startApp" . fun $ \_ _ _ -> cb


stage :: MonadJSM m => SnabbdomT a m RawNode
stage = liftJSM $ fromJSValUnchecked =<< jsg "container"

