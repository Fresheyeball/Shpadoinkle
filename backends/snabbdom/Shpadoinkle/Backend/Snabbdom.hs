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
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


{-|
   This module provides the 'Backend' binding for the JavaScript virtual DOM implementation <https://github.com/snabbdom/snabbdom Snabbdom>.
-}


module Shpadoinkle.Backend.Snabbdom
  ( SnabbdomT (..)
  , runSnabbdom
  , stage
  ) where


import           Control.Category            ((.))
import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.Catch         (MonadCatch, MonadThrow)
import           Control.Monad.Reader        (MonadIO, MonadReader (..),
                                              MonadTrans, ReaderT (..), forM_,
                                              void, (>=>))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl,
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Data.FileEmbed              (embedStringFile)
import           Data.Text                   (Text, split)
import           Data.Traversable            (for)
import           GHCJS.DOM                   (currentDocumentUnchecked)
import           GHCJS.DOM.Document          (createElement, getBodyUnsafe)
import           GHCJS.DOM.Element           (setAttribute)
import           GHCJS.DOM.Node              (appendChild)
import           Language.Javascript.JSaddle hiding (JSM, MonadJSM, liftJSM,
                                              (#))
import           Prelude                     hiding (id, (.))
import           UnliftIO                    (MonadUnliftIO (..), TVar,
                                              UnliftIO (UnliftIO, unliftIO),
                                              withUnliftIO)
import           UnliftIO.Concurrent         (forkIO)

import           Shpadoinkle                 hiding (children, name, props)


default (Text)


newtype SnabVNode = SnabVNode { unVNode :: JSVal }
instance ToJSVal   SnabVNode where toJSVal   = return . unVNode
instance FromJSVal SnabVNode where fromJSVal = return . Just . SnabVNode


newtype SnabbdomT model m a = Snabbdom { unSnabbdom :: ReaderT (TVar model) m a }
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader (TVar model)
  , MonadTrans
  , MonadTransControl
  , MonadThrow
  , MonadCatch
  )


#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (SnabbdomT model m)
#endif


instance MonadBase n m => MonadBase n (SnabbdomT model m) where
  liftBase = liftBaseDefault


instance MonadBaseControl n m => MonadBaseControl n (SnabbdomT model m) where
  type StM (SnabbdomT model m) a = ComposeSt (SnabbdomT model) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM


instance MonadUnliftIO m => MonadUnliftIO (SnabbdomT r m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = Snabbdom . ReaderT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runReaderT r . unSnabbdom))
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    Snabbdom . ReaderT $ \r ->
    withRunInIO $ \run' ->
    inner (run' . flip runReaderT r . unSnabbdom)


-- | 'SnabbdomT' is a @newtype@ of 'ReaderT', this is the 'runReaderT' equivalent.
runSnabbdom :: TVar model -> SnabbdomT model m ~> m
runSnabbdom t (Snabbdom r) = runReaderT r t


props :: Monad m => (m ~> JSM) -> TVar a -> [(Text, Prop (SnabbdomT a m) a)] -> JSM Object
props toJSM i xs = do
  o <- create
  propsObj     <- create
  listenersObj <- create
  classesObj   <- create
  attrsObj     <- create
  hooksObj     <- create
  void $ xs `for` \(k, p) -> case p of
    PData d -> unsafeSetProp (toJSString k) d propsObj
    PPotato pot -> do
      f' <- toJSVal . fun $ \_ _ ->
        let
          g vnode = do
            vnode' <- valToObject vnode
            stm <- pot . RawNode =<< unsafeGetProp "elm" vnode'
            let go = atomically stm >>= writeUpdate i . hoist (toJSM . runSnabbdom i)
            void $ forkIO go
        in \case
          [vnode]    -> g vnode
          [_, vnode] -> g vnode
          _          -> return ()
      unsafeSetProp "insert" f' hooksObj
      unsafeSetProp "update" f' hooksObj

    PText t -> do
      t' <- toJSVal t
      true <- toJSVal True
      case k of
        "className" | t /= "" -> forM_ (split (== ' ') t) $ \u ->
          if u == mempty then pure () else unsafeSetProp (toJSString u) true classesObj
        "style"     | t /= "" -> unsafeSetProp (toJSString k) t' attrsObj
        "type"      | t /= "" -> unsafeSetProp (toJSString k) t' attrsObj
        "autofocus" | t /= "" -> unsafeSetProp (toJSString k) t' attrsObj
        _                     -> unsafeSetProp (toJSString k) t' propsObj

    PListener f -> do
      f' <- toJSVal . fun $ \_ _ -> \case
        [] -> return ()
        ev:_ -> do
          rn <- unsafeGetProp "target" =<< valToObject ev
          x <- f (RawNode rn) (RawEvent ev)
          writeUpdate i $ hoist (toJSM . runSnabbdom i) x
      unsafeSetProp (toJSString k) f' listenersObj

    PFlag b -> do
      f <- toJSVal b
      unsafeSetProp (toJSString k) f propsObj

  p  <- toJSVal propsObj
  l  <- toJSVal listenersObj
  k  <- toJSVal classesObj
  a  <- toJSVal attrsObj
  h' <- toJSVal hooksObj
  unsafeSetProp "props" p  o
  unsafeSetProp "class" k  o
  unsafeSetProp "on"    l  o
  unsafeSetProp "attrs" a  o
  unsafeSetProp "hook"  h' o
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
        _   -> return ())
      unsafeSetProp "insert" ins hook
      hoo <- toJSVal hook
      unsafeSetProp "hook" hoo o
      fromJSValUnchecked =<< jsg2 "vnode" "div" o


  patch :: RawNode -> Maybe SnabVNode -> SnabVNode -> SnabbdomT a m SnabVNode
  patch (RawNode r) f t = t <$ (liftJSM . void $ jsg2 "patchh" f' t)
    where f' = maybe r unVNode f


  setup :: JSM () -> JSM ()
  setup cb = do
    void $ eval @Text $(embedStringFile "Shpadoinkle/Backend/Snabbdom/Setup.js")
    void . jsg1 "startApp" . fun $ \_ _ _ -> cb


-- | Get the @window.container@ DOM node produced by 'setup' (@Setup.js@).
stage :: MonadJSM m => SnabbdomT a m RawNode
stage = liftJSM $ do
  doc <- currentDocumentUnchecked
  elm <- createElement doc ("div" :: Text)
  setAttribute elm ("id" :: Text) ("stage" :: Text)
  b <- getBodyUnsafe doc
  _ <- appendChild b elm
  RawNode <$> toJSVal elm

