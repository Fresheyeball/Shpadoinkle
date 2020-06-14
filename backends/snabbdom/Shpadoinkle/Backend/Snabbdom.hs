{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Shpadoinkle.Backend.Snabbdom
  ( SnabbdomT (..)
  , runSnabbdom
  , stage
  ) where

import           Control.Category
import           Control.Monad.Reader
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


#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (SnabbdomT model m)
#endif


runSnabbdom :: TVar model -> SnabbdomT model m a -> m a
runSnabbdom t (Snabbdom r) = runReaderT r t


props :: Monad m => (m ~> JSM) -> TVar a -> [(Text, Prop (SnabbdomT a m) a)] -> JSM Object
props toJSM i xs = do
  o <- create
  a <- create
  e <- create
  c <- create
  void $ xs `for` \(k, p) -> case p of
    PText t -> do
      t' <- toJSVal t
      true <- toJSVal True
      case k of
        "className" | t /= "" -> unsafeSetProp (toJSString t) true c
        _                     -> unsafeSetProp (toJSString k) t' a
    PListener f -> do
      f' <- toJSVal . fun $ \_ _ -> \case
        [] -> return ()
        ev:_ -> do
          rn <- unsafeGetProp "target" =<< valToObject ev
          writeUpdate i . const . fmap (convertC (toJSM . runSnabbdom i))
                             $ f (RawNode rn) (RawEvent ev)
      unsafeSetProp (toJSString k) f' e
    PFlag b -> do
      f <- toJSVal b
      unsafeSetProp (toJSString k) f a

  p <- toJSVal a
  l <- toJSVal e
  k <- toJSVal c
  unsafeSetProp "props" p o
  unsafeSetProp "class" k o
  unsafeSetProp "on"    l o
  return o


instance (MonadJSM m, Eq a) => Backend (SnabbdomT a) m a where
  type VNode (SnabbdomT a) m = SnabVNode

  interpret :: (m ~> JSM) -> Html (SnabbdomT a m) a -> SnabbdomT a m SnabVNode
  interpret toJSM = \case

    TextNode t -> liftJSM $ fromJSValUnchecked =<< toJSVal t

    Node name ps [TextNode t] -> do
      i <- ask; liftJSM $ do
        o <- props toJSM i ps
        fromJSValUnchecked =<< jsg3 "vnode" name o t

    Node name ps children -> do
      i <- ask; liftJSM $ do
        o <- props toJSM i ps
        traverse ((toJSM . runSnabbdom i) . interpret toJSM >=> toJSVal) children
          >>= jsg3 "vnode" name o >>= fromJSValUnchecked

    Potato mrn -> liftJSM $ do
      o <- create
      hook <- create
      rn <- mrn
      ins <- toJSVal =<< function (\_ _ -> \case
        [n] -> void $ jsg2 "potato" n rn
        _ -> return ())
      unsafeSetProp "insert" ins hook
      hoo <- toJSVal hook
      unsafeSetProp "hook" hoo o
      fromJSValUnchecked =<< jsg2 "vnode" "div" o


  patch :: RawNode -> Maybe SnabVNode -> SnabVNode -> SnabbdomT a m SnabVNode
  patch (RawNode r) f t = t <$ (liftJSM . void $ jsg2 "patchh" f' t)
    where f' = maybe r unVNode f


  setup :: JSM () -> SnabbdomT a m ()
  setup cb = liftJSM $ do
    void $ eval @Text $(embedStringFile "Shpadoinkle/Backend/Snabbdom/Setup.js")
    void . jsg1 "startApp" . fun $ \_ _ _ -> cb


stage :: MonadJSM m => SnabbdomT a m RawNode
stage = liftJSM $ fromJSValUnchecked =<< jsg "container"

