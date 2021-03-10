{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
import           Control.Monad.Catch         (MonadCatch, MonadThrow, MonadMask)
import           Control.Monad.Reader        (MonadIO, MonadReader (..),
                                              MonadTrans, ReaderT (..), forM_,
                                              void)
import           Control.Monad.Writer        (MonadWriter)
import           Control.Monad.State         (MonadState)
import           Control.Monad.RWS           (MonadRWS)
import           Control.Monad.Except        (MonadError)
import           Control.Monad.Cont          (MonadCont)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl,
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Data.FileEmbed              (embedStringFile)
import           Data.Map.Internal           (Map (Bin, Tip))
import           Data.Text                   (Text, words)
import           GHCJS.DOM                   (currentDocumentUnchecked)
import           GHCJS.DOM.Document          (createElement, getBodyUnsafe)
import           GHCJS.DOM.Element           (setAttribute)
import           GHCJS.DOM.Node              (appendChild)
import           Language.Javascript.JSaddle hiding (JSM, MonadJSM, liftJSM,
                                              (#))
import           Prelude                     hiding (id, words, (.))
import           UnliftIO                    (MonadUnliftIO (..), TVar,
                                              UnliftIO (UnliftIO, unliftIO),
                                              withUnliftIO)
import           UnliftIO.Concurrent         (forkIO)

import           Shpadoinkle


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
  , MonadMask
  , MonadWriter w
  , MonadState s
  , MonadRWS (TVar model) w s
  , MonadError e
  , MonadCont
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


traverseWithKey_ :: Applicative t => (k -> a -> t ()) -> Map k a -> t ()
traverseWithKey_ f = go
  where
    go Tip             = pure ()
    go (Bin 1 k v _ _) = f k v
    go (Bin _ k v l r) = go l *> f k v *> go r
{-# INLINE traverseWithKey_ #-}


props :: Monad m => NFData a => (m ~> JSM) -> TVar a -> Props (SnabbdomT a m) a -> JSM Object
props toJSM i (Props xs) = do
  o <- create
  propsObj     <- create
  listenersObj <- create
  classesObj   <- create
  attrsObj     <- create
  hooksObj     <- create
  flip traverseWithKey_ xs $ \k p ->
    let k' = toJSString k
    in  case p of
      PData d -> unsafeSetProp k' d propsObj
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

      PText t
        | k == "className" -> forM_ (words t) $ \u ->
            unsafeSetProp (toJSString u) jsTrue classesObj
        | t /= "" -> do
            t' <- valMakeText t
            unsafeSetProp k' t' $ case k of
              "style"     -> attrsObj
              "type"      -> attrsObj
              "autofocus" -> attrsObj
              "checked"   -> attrsObj
              _           -> propsObj
        | otherwise -> do
            t' <- valMakeText t
            unsafeSetProp k' t' propsObj

      PListener f -> do
        f' <- toJSVal . fun $ \_ _ -> \case
          [] -> return ()
          ev:_ -> do
            rn <- unsafeGetProp "target" =<< valToObject ev
            x <- f (RawNode rn) (RawEvent ev)
            writeUpdate i $ hoist (toJSM . runSnabbdom i) x
        unsafeSetProp k' f' listenersObj

      PFlag b ->
        unsafeSetProp k' (toJSBool b) propsObj

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


instance (MonadJSM m, NFData a) => Backend (SnabbdomT a) m a where
  type VNode (SnabbdomT a) m = SnabVNode

  interpret :: (m ~> JSM) -> Html (SnabbdomT a m) a -> SnabbdomT a m SnabVNode
  interpret toJSM (Html h') = h' mkNode mkPotato mkText
    where
      mkNode name ps children = do
        i <- ask; liftJSM $ do
          !o <- props toJSM i $ toProps ps
          !cs <- toJSM . runSnabbdom i $ sequence children
          SnabVNode <$> jsg3 "vnode" name o cs

      mkPotato mrn = liftJSM $ do
        o <- create
        hook <- create
        rn <- mrn
        ins <- toJSVal =<< function (\_ _ -> \case
          [n] -> void $ jsg2 "potato" n rn
          _   -> return ())
        unsafeSetProp "insert" ins hook
        hoo <- toJSVal hook
        unsafeSetProp "hook" hoo o
        SnabVNode <$> jsg2 "vnode" "div" o

      mkText = liftJSM . fmap SnabVNode . valMakeText


  patch :: RawNode -> Maybe SnabVNode -> SnabVNode -> SnabbdomT a m SnabVNode
  patch (RawNode container) mPreviousNode newNode = liftJSM $ newNode <$ jsg2 "patchh" previousNode newNode
    where previousNode = maybe container unVNode mPreviousNode


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

