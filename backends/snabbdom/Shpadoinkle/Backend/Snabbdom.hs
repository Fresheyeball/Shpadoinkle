{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
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
import           Data.Function               ((&))
#ifdef ghcjs_HOST_OS
import           Control.Monad               (join)
#endif
import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Cont          (MonadCont)
import           Control.Monad.Except        (MonadError)
import           Control.Monad.Reader        (MonadIO, MonadReader (..),
                                              MonadTrans, ReaderT (..), forM_,
                                              void)
import           Control.Monad.State         (MonadState)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl,
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Control.Monad.Writer        (MonadWriter)
import           Data.FileEmbed              (embedStringFile)
import           Data.Map.Internal           (Map (Bin, Tip))
import           Data.Text                   (Text, isPrefixOf, words)
import           Prelude                     hiding (id, words, (.))
import           Shpadoinkle.JSFFI           (JSObject, JSVal, appendChild,
                                              body, createElement, eval,
                                              getProp, global, jsAs, jsTo,
                                              jsTrue, mkEmptyObject, mkFun',
                                              setId, setInnerHTML, setProp, (#),
                                              (#-))
#ifdef ghcjs_HOST_OS
import           Shpadoinkle.JSFFI           (jsAs)
#endif
import           UnliftIO                    (MonadUnliftIO (..), TVar,
                                              UnliftIO (UnliftIO, unliftIO),
                                              withUnliftIO)
import           UnliftIO.Concurrent         (forkIO)

import           Shpadoinkle


default (Text)


newtype SnabbdomT model m a = Snabbdom { unSnabbdom :: ReaderT (TVar model) m a }
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadTrans
  , MonadTransControl
  , MonadThrow
  , MonadCatch
  , MonadMask
  , MonadWriter w
  , MonadState s
  , MonadError e
  , MonadCont
  )


snabbAsk :: Monad m => SnabbdomT model m (TVar model)
snabbAsk = Snabbdom ask


instance MonadBase n m => MonadBase n (SnabbdomT model m) where
  liftBase = liftBaseDefault


instance MonadReader r m => MonadReader r (SnabbdomT a m) where
  ask = Snabbdom . ReaderT $ const ask
  local f (Snabbdom rs) = Snabbdom $ ReaderT $ local f . runReaderT rs


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
{-# SPECIALIZE traverseWithKey_ :: (k -> a -> JSM ()) -> Map k a -> JSM () #-}


newtype SnabVNode = SnabVNode { unVNode :: JSVal }
-- WANT: strengthen to some JSNode type?


-- | Insert function @f@ in @o@ as field @k@, after already running an existing property function if it exists.
insertHook :: Text -- ^ @k@
           -> JSVal -- ^ @f@
           -> JSObject -- ^ @o@
           -> JSM ()
#ifndef ghcjs_HOST_OS
insertHook t f' hooksObj = global #- "insertHook" $ (t, f', hooksObj)
#else
insertHook t f' hooksObj = insertHook' (jsAs t) f' (jsAs hooksObj)
foreign import javascript unsafe "window['insertHook']($1,$2,$3)" insertHook' :: JSVal -> JSVal -> JSVal -> JSM ()
#endif


-- | Interpret uninterpreted props into a Snabbdom-formatted JavaScript object
{-# SPECIALIZE props :: NFData a => (JSM ~> JSM) -> TVar a -> Props (SnabbdomT a JSM) a -> JSM JSObject #-}
props :: Monad m => NFData a => (m ~> JSM) -> TVar a -> Props (SnabbdomT a m) a -> JSM JSObject
props toJSM i (Props xs) = do
  o <- mkEmptyObject
  propsObj     <- mkEmptyObject
  listenersObj <- mkEmptyObject
  classesObj   <- mkEmptyObject
  attrsObj     <- mkEmptyObject
  hooksObj     <- mkEmptyObject
  flip traverseWithKey_ xs $ \k p ->
    let k' = k
    in  case p of
      PData d -> setProp k' d propsObj
      PPotato pot -> do
        f' <- mkFun' $
          let
            g vnode'' = do
              vnode_ <- jsTo @JSObject vnode''
              stm <- pot . RawNode =<< getProp "elm" vnode_
              let go = atomically stm >>= writeUpdate i . hoist (toJSM . runSnabbdom i)
              void $ forkIO go
          in \case
            [vnode_]    -> g vnode_
            [_, vnode_] -> g vnode_
            _           -> return ()
        insertHook "insert" (jsAs f') hooksObj
        insertHook "update" (jsAs f') hooksObj
      PText t
        | k == "className" -> forM_ (words t) $ \u ->
            classesObj & setProp u jsTrue
        | t /= "" -> do
            setProp k' t $ case k of
              "style"                    -> attrsObj
              "type"                     -> attrsObj
              "autofocus"                -> attrsObj
              "checked"                  -> attrsObj
              d | "data-" `isPrefixOf` d -> attrsObj
              _                          -> propsObj
        | otherwise -> do
            setProp k' t propsObj

      PListener f -> do
        f' <- mkFun' $ \case
          [] -> return ()
          ev:_ -> do
            rn <- getProp "target" =<< jsTo @JSObject ev
            ev' <- jsTo ev
            x <- f (RawNode rn) (RawEvent ev')
            writeUpdate i $ hoist (toJSM . runSnabbdom i) x
        setProp k' f' listenersObj

      PFlag b ->
        setProp k' b propsObj

  o & setProp "props" propsObj
  o & setProp "class" classesObj
  o & setProp "on"    listenersObj
  o & setProp "attrs" attrsObj
  o & setProp "hook"  hooksObj
  return o


-- | Call-site for Snabbdom's @h()@ function
vnode :: Text -> JSObject -> [SnabVNode] -> JSM SnabVNode
#ifndef ghcjs_HOST_OS
vnode name o cs = SnabVNode <$> ( global # "vnode" $ (name, o, unVNode <$> cs) )
#else
vnode t o cs = vnode' t o (jsAs @JSVal $ unVNode <$> cs)
foreign import javascript unsafe "window['vnode']($1,$2,$3)" vnode' :: Text -> JSObject -> JSVal -> JSM SnabVNode
#endif


-- | Alternative invocation of Snabbdom's @h()@ function for potatos, where there are no children
#ifndef ghcjs_HOST_OS
vnodePotato :: JSObject -> JSM SnabVNode
vnodePotato o = SnabVNode <$> ( global # "vnode" $ ("div", o) )
#else
foreign import javascript unsafe "window['vnode']('div',$1)" vnodePotato :: JSObject -> JSM SnabVNode
#endif


-- | Call-site for Snabbdom's @patch()@ function
patchh :: JSObject -> SnabVNode -> JSM ()
#ifndef ghcjs_HOST_OS
patchh previousNode (SnabVNode newNode) = global #- "patchh" $ (previousNode, newNode)
#else
patchh p (SnabVNode n) = patchh' (jsAs p) n
foreign import javascript unsafe "window['patchh']($1,$2)" patchh' :: JSVal -> JSVal -> JSM ()
#endif


instance (MonadJSM m, NFData a) => Backend (SnabbdomT a) m a where
  type VNode (SnabbdomT a) m = SnabVNode

  interpret :: (m ~> JSM) -> Html (SnabbdomT a m) a -> SnabbdomT a m SnabVNode
  interpret toJSM (Html h') = h' mkNode mkPotato mkText
    where
      mkNode name ps children = do
        i <- snabbAsk; liftJSM $ do
          !o <- props toJSM i $ toProps ps
          !cs <- toJSM . runSnabbdom i $ sequence children
          vnode name o cs

      mkPotato mrn = snabbAsk >>= \i -> liftJSM $ do
        (RawNode rn, stm) <- mrn
        ins <- mkFun' (\case
          [n] -> do
            elm' :: JSObject <- getProp "elm" =<< jsTo @JSObject n
            elm' #- "appendChild" $ rn
          _   -> return ())
        hook <- mkEmptyObject
        setProp "insert" ins hook
        classes <- mkEmptyObject
        setProp "potato" jsTrue classes
        o <- mkEmptyObject
        o & setProp "hook" hook
        o & setProp "classes" classes
        let go = atomically stm >>= writeUpdate i . hoist (toJSM . runSnabbdom i) >> go
        void $ forkIO go
        vnodePotato o

      mkText t = liftJSM $ pure . SnabVNode . jsAs =<< htmlDecode (jsAs t)


  patch :: RawNode -> Maybe SnabVNode -> SnabVNode -> SnabbdomT a m SnabVNode
  patch (RawNode container) mPreviousNode newNode = liftJSM $ do
    previousNode <- maybe (pure container) (jsTo @JSObject . unVNode) mPreviousNode
    newNode <$ patchh previousNode newNode


  setup :: JSM () -> JSM ()
  setup cb = do
    void $ eval ($(embedStringFile "Shpadoinkle/Backend/Snabbdom/Setup.js") :: Text)
    startApp cb


-- | Generate the call-site bindings for Snabbdom in @window@
startApp :: JSM () -> JSM ()
startApp cb = do
  f <- mkFun' $ const cb
  global #- "startApp" $ f


-- | Get the @<body>@ DOM node after emptying it.
stage :: MonadJSM m => SnabbdomT a m RawNode
stage = liftJSM $ do
  placeholder <- createElement ("div" :: Text)
  placeholder & setId ("stage" :: Text)
  body & setInnerHTML ""
  _ <- body & appendChild placeholder
  pure $ RawNode (jsAs placeholder)
{-# SPECIALIZE stage :: SnabbdomT a JSM RawNode #-}

