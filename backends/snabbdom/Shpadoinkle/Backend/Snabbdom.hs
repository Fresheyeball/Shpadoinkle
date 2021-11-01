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
import           GHCJS.DOM                   (currentDocumentUnchecked)
import           GHCJS.DOM.Document          (createElement, getBodyUnsafe)
import           GHCJS.DOM.Element           (setId, setInnerHTML)
import           GHCJS.DOM.Node              (appendChild)
#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle (FromJSVal (..), JSVal, Object,
                                              ToJSVal (..), create, eval, fun,
                                              function, jsTrue, jsg1, jsg2,
                                              jsg3, makeObject, toJSBool,
                                              toJSString, unsafeGetProp,
                                              unsafeSetProp, valMakeString,
                                              valMakeText, valToObject, (!),
                                              (#))
#else
import           Language.Javascript.JSaddle (FromJSVal (..), JSVal, Object,
                                              ToJSVal (..), create, eval, fun,
                                              function, jsTrue, makeObject,
                                              toJSBool, toJSString,
                                              unsafeGetProp, unsafeSetProp,
                                              valMakeString, valMakeText,
                                              valToObject, (!), (#))
#endif
import           Prelude                     hiding (id, words, (.))
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


#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (SnabbdomT model m)
#endif


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
instance ToJSVal   SnabVNode where toJSVal   = return . unVNode
instance FromJSVal SnabVNode where fromJSVal = return . Just . SnabVNode


-- | Insert function @f@ in @o@ as field @k@, after already running an existing property function if it exists.
insertHook :: Text -- ^ @k@
           -> JSVal -- ^ @f@
           -> Object -- ^ @o@
           -> JSM ()
#ifndef ghcjs_HOST_OS
insertHook t f' hooksObj = void $ jsg3 "insertHook" t f' hooksObj
#else
insertHook t f' hooksObj = join $ insertHook' <$> toJSVal t <*> pure f' <*> toJSVal hooksObj
foreign import javascript unsafe "window['insertHook']($1,$2,$3)" insertHook' :: JSVal -> JSVal -> JSVal -> JSM ()
#endif


-- | Interpret uninterpreted props into a Snabbdom-formatted JavaScript object
{-# SPECIALIZE props :: NFData a => (JSM ~> JSM) -> TVar a -> Props (SnabbdomT a JSM) a -> JSM Object #-}
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
            g vnode'' = do
              vnode_ <- valToObject vnode''
              stm <- pot . RawNode =<< unsafeGetProp "elm" vnode_
              let go = atomically stm >>= writeUpdate i . hoist (toJSM . runSnabbdom i)
              void $ forkIO go
          in \case
            [vnode_]    -> g vnode_
            [_, vnode_] -> g vnode_
            _           -> return ()
        insertHook "insert" f' hooksObj
        insertHook "update" f' hooksObj
      PText t
        | k == "className" -> forM_ (words t) $ \u ->
            unsafeSetProp (toJSString u) jsTrue classesObj
        | t /= "" -> do
            t' <- valMakeText t
            unsafeSetProp k' t' $ case k of
              "style"                    -> attrsObj
              "type"                     -> attrsObj
              "autofocus"                -> attrsObj
              "checked"                  -> attrsObj
              d | "data-" `isPrefixOf` d -> attrsObj
              _                          -> propsObj
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


-- | Call-site for Snabbdom's @h()@ function
vnode :: Text -> Object -> [SnabVNode] -> JSM SnabVNode
#ifndef ghcjs_HOST_OS
vnode name o cs = SnabVNode <$> jsg3 "vnode" name o cs
#else
vnode t o cs    = join $ vnode' <$> pure t <*> pure o <*> toJSVal cs
foreign import javascript unsafe "window['vnode']($1,$2,$3)" vnode' :: Text -> Object -> JSVal -> JSM SnabVNode
#endif


-- | Alternative invocation of Snabbdom's @h()@ function for potatos, where there are no children
#ifndef ghcjs_HOST_OS
vnodePotato :: Object -> JSM SnabVNode
vnodePotato o = SnabVNode <$> jsg2 "vnode" "div" o
#else
foreign import javascript unsafe "window['vnode']('div',$1)" vnodePotato :: Object -> JSM SnabVNode
#endif


-- | Call-site for Snabbdom's @patch()@ function
patchh :: JSVal -> SnabVNode -> JSM ()
#ifndef ghcjs_HOST_OS
patchh previousNode newNode = void $ jsg2 "patchh" previousNode newNode
#else
patchh p (SnabVNode n)      = patchh' p n
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
        ins <- toJSVal =<< function (\_ _ -> \case
          [n] -> do
            elm' <- (! "elm") =<< makeObject n
            void $ elm' # "appendChild" $ rn
          _   -> return ())
        hook <- create
        unsafeSetProp "insert" ins hook
        classes <- create
        unsafeSetProp "potato" jsTrue classes
        o <- create
        flip (unsafeSetProp "hook") o =<< toJSVal hook
        flip (unsafeSetProp "classes") o =<< toJSVal classes
        let go = atomically stm >>= writeUpdate i . hoist (toJSM . runSnabbdom i) >> go
        void $ forkIO go
        vnodePotato o

      mkText t = liftJSM . fmap SnabVNode $ valMakeString =<< htmlDecode (toJSString t)


  patch :: RawNode -> Maybe SnabVNode -> SnabVNode -> SnabbdomT a m SnabVNode
  patch (RawNode container) mPreviousNode newNode = liftJSM $ newNode <$ patchh previousNode newNode
    where previousNode = maybe container unVNode mPreviousNode


  setup :: JSM () -> JSM ()
  setup cb = do
    void $ eval @Text $(embedStringFile "Shpadoinkle/Backend/Snabbdom/Setup.js")
    startApp cb


-- | Generate the call-site bindings for Snabbdom in @window@
startApp :: JSM () -> JSM ()
#ifndef ghcjs_HOST_OS
startApp cb = void . jsg1 "startApp" . fun $ \_ _ _ -> cb
#else
startApp cb = startApp' =<< toJSVal =<< function (fun $ \_ _ _ -> cb)
foreign import javascript unsafe "window['startApp']($1)" startApp' :: JSVal -> JSM ()
#endif


-- | Get the @<body>@ DOM node after emptying it.
stage :: MonadJSM m => SnabbdomT a m RawNode
stage = liftJSM $ do
  doc <- currentDocumentUnchecked
  placeholder <- createElement doc ("div" :: Text)
  setId placeholder ("stage" :: Text)
  b <- getBodyUnsafe doc
  setInnerHTML b ""
  _ <- appendChild b placeholder
  RawNode <$> toJSVal placeholder
{-# SPECIALIZE stage :: SnabbdomT a JSM RawNode #-}

