{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

#ifndef ghcjs_HOST_OS
{-# LANGUAGE StandaloneDeriving         #-}
#endif


{-|
   This backend is to serve as a canonical representation of a well-behaved
   backend. Defining well-behaved in the context of web development
   is rather difficult and complex.

   The rules of a backend are informal. Roughly, if we give the backend
   some Html, we expect it to update the DOM at runtime in the way we expect.

   Since this is canonical, all other backends are expected to behave
   identically to this one. If differences exist they should be patched
   so that we retain renderer polymorphism, such that we can change out
   the renderer of our application; without updating the application logic
   with confidence it will behave as expected.
-}


module Shpadoinkle.Backend.ParDiff
  ( ParDiffT (..)
  , runParDiff
  , stage
  ) where


import           Control.Applicative         (Alternative)
import           Control.Monad               (forM_, void, when)
import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Cont          (MonadCont)
import           Control.Monad.Except        (MonadError)
import           Control.Monad.RWS           (MonadRWS)
import           Control.Monad.Reader        (MonadIO, MonadReader (ask),
                                              MonadTrans (..), ReaderT (..),
                                              guard)
import           Control.Monad.State         (MonadState)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl,
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Control.Monad.Writer        (MonadWriter)
import           Data.Kind                   (Type)
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Map.Internal           (Map (Bin, Tip))
import           Data.Maybe                  (isJust)
import           Data.Monoid                 ((<>))
import           Data.Once                   (Once, newOnce, runOnce)
import           Data.Text                   (Text, intercalate)
import           Language.Javascript.JSaddle (FromJSVal (fromJSValUnchecked),
                                              JSString, MakeObject (makeObject),
                                              Object, ToJSString (toJSString),
                                              ToJSVal (toJSVal), eval, fun,
                                              jsFalse, jsTrue, jsg, jsg2,
                                              liftJSM, unsafeSetProp,
                                              valMakeText, (#))
import           UnliftIO                    (MonadUnliftIO (..), TVar,
                                              UnliftIO (UnliftIO, unliftIO),
                                              withUnliftIO)
import           UnliftIO.Concurrent         (forkIO)
import           UnliftIO.STM                (STM, atomically)

import           Shpadoinkle                 (Backend (..), Continuation,
                                              Html (..), JSM, MonadJSM, NFData,
                                              Prop (..), Props (..),
                                              RawEvent (RawEvent),
                                              RawNode (RawNode, unRawNode),
                                              hoist, htmlDecode, toProps,
                                              type (~>), writeUpdate)


default (Text)


newtype ParDiffT model m a = ParDiffT { unParDiff :: ReaderT (TVar model) m a }
  deriving
  ( Functor
  , Applicative
  , Alternative
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
deriving instance MonadJSM m => MonadJSM (ParDiffT model m)
#endif


instance MonadBase n m => MonadBase n (ParDiffT model m) where
  liftBase = liftBaseDefault


instance MonadBaseControl n m => MonadBaseControl n (ParDiffT model m) where
  type StM (ParDiffT model m) a = ComposeSt (ParDiffT model) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM


instance MonadUnliftIO m => MonadUnliftIO (ParDiffT r m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = ParDiffT . ReaderT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runReaderT r . unParDiff))
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    ParDiffT . ReaderT $ \r ->
    withRunInIO $ \run' ->
    inner (run' . flip runReaderT r . unParDiff)


-- | 'ParDiffT' is a @newtype@ of 'ReaderT', this is the 'runReaderT' equivalent.
runParDiff :: TVar model -> ParDiffT model m ~> m
runParDiff t (ParDiffT r) = runReaderT r t


data ParVNode :: Type -> Type where
  ParNode     :: Once JSM RawNode -> {-# UNPACK #-} !Text -> ParVProps a -> [ParVNode a] -> ParVNode a
  ParPotato   :: Once JSM RawNode -> ParVNode a
  ParTextNode :: Once JSM RawNode -> {-# UNPACK #-} !Text -> ParVNode a

type ParVProps = Props JSM

type ParVProp = Prop JSM


props :: Monad m => NFData a => (m ~> JSM) -> TVar a -> Props (ParDiffT a m) a -> RawNode -> JSM ()
props toJSM i (Props ps) (RawNode raw) = do
  raw' <- makeObject raw
  traverseWithKey_ (prop toJSM i raw') ps


prop :: Monad m => NFData a => (m ~> JSM) -> TVar a -> Object -> Text -> Prop (ParDiffT a m) a -> JSM ()
prop toJSM i raw k = \case
  PData d     -> unsafeSetProp k' d raw
  PText t     -> do
    t' <- valMakeText t
    unsafeSetProp k' t' raw
  PPotato p   -> setProptado i (fmap (fmap (hoist (toJSM . runParDiff i))) . p) raw
  PListener f -> setListener i (\x y -> hoist (toJSM . runParDiff i) <$> f x y) raw k'
  PFlag True  -> unsafeSetProp k' jsTrue raw
  PFlag False -> return ()
  where
    k' = toJSString k


setProptado :: forall a. NFData a => TVar a -> (RawNode -> JSM (STM (Continuation JSM a))) -> Object -> JSM ()
setProptado i f o = do
  elm <- RawNode <$> toJSVal o
  stm <- f elm
  let go = atomically stm >>= writeUpdate i >> go
  void $ forkIO go


setListener :: NFData a => TVar a -> (RawNode -> RawEvent -> JSM (Continuation JSM a)) -> Object -> JSString -> JSM ()
setListener i m o k = do
  elm <- RawNode <$> toJSVal o
  f <- toJSVal . fun $ \_ _ -> \case
    e:_ -> do
      x <- m elm (RawEvent e)
      writeUpdate i x
    _ -> return ()
  unsafeSetProp ("on" <> k) f o


getRaw :: ParVNode a -> Once JSM RawNode
getRaw = \case
  ParNode mk _ _ _ -> mk
  ParPotato mk     -> mk
  ParTextNode mk _ -> mk


makeProp :: Monad m => (m ~> JSM) -> TVar a -> Prop (ParDiffT a m) a -> ParVProp a
makeProp toJSM i = \case
  PText t     -> PText t
  PData t     -> PData t
  PPotato p   -> PPotato $ fmap (fmap (hoist (toJSM . runParDiff i))) . p
  PListener m -> PListener $ \x y -> hoist (toJSM . runParDiff i) <$> m x y
  PFlag b     -> PFlag b


setup' :: JSM () -> JSM ()
setup' cb = do
  void $ eval $ intercalate "\n"
    [ " window.deleteProp = (k, obj) => {"
    , "   delete obj[k]"
    , " }"
    , " window.container = document.createElement('div')"
    , " document.body.appendChild(container)"
    ]
  cb


setFlag :: Object -> Text -> Bool -> JSM ()
setFlag obj' k b
  | b = unsafeSetProp k' jsTrue obj'
  | otherwise = case k of
    "checked"  -> unsafeSetProp k' jsFalse obj'
    "disabled" -> void (obj' # "removeAttribute" $ "disabled")
    _          -> void (jsg2 "deleteProp" k' obj')
  where
    k' = toJSString k


traverseWithKey_ :: Applicative t => (k -> a -> t ()) -> Map k a -> t ()
traverseWithKey_ f = go
  where
    go Tip             = pure ()
    go (Bin 1 k v _ _) = f k v
    go (Bin _ k v l r) = go l *> f k v *> go r


managePropertyState :: NFData a => TVar a -> Object -> ParVProps a -> ParVProps a -> JSM ()
managePropertyState i obj' (Props !old) (Props !new) = void $ do
  -- The following step may be necessary if the old DOM and the new VDOM both have checked == False
  -- but the user just checked this checkbox / radio button and the browser set its
  -- checked property to true without setting its checked attribute.
  -- As far as we know this issue only occurs with the checked property.
  -- As far as we know this issue only occurs with the value properties of input controls,
  -- which include but are not necessarily limited to:
  --  * The value properties of input, select, and textarea elements
  --  * The checked properties of input type={checkbox,radio} elements
  --  * The src properties of image elements
  --  * The files properties of file elements
  -- Of these properties, checked is the only one where we know that the absence of the attribute
  -- in both the old and new (V)DOMs is consistent with the property needing to be updated
  -- because the property was updated with the corresponding attribute being absent the whole time.
  let isFalseFlag (PFlag f) = not f
      isFalseFlag _         = False
  when (isJust (M.lookup "checked" new >>= guard . isFalseFlag))
    (unsafeSetProp "checked" jsFalse obj')

  let toRemove = M.difference old new
      willInclude new' old'
        | new' == old' = Nothing
        | otherwise    = Just new'
      toInclude = M.differenceWith willInclude new old

      remove k _ = case k of
        "className" -> void $ obj' # "removeAttribute" $ "class"
        "href"      -> void $ obj' # "removeAttribute" $ "href"
        "htmlFor"   -> void $ obj' # "removeAttribute" $ "for"
        "style"     -> void $ obj' # "removeAttribute" $ "style"
        "checked"   -> unsafeSetProp (toJSString k) jsFalse obj'
        "disabled"  -> void $ obj' # "removeAttribute" $ "disabled"
        _           -> void $ jsg2 "deleteProp" (toJSString k) obj'

  traverseWithKey_ remove toRemove

  let include k v =
        let k' = toJSString k
        in  case v of
          PPotato p -> void . p . RawNode =<< toJSVal obj' -- FIXME why throw away continuation...???
          PData j -> unsafeSetProp k' j obj'
          -- new text prop, set
          PText t -> do
            t' <- valMakeText t
            unsafeSetProp k' t' obj'
          -- new flag prop, set
          PFlag b -> setFlag obj' k b
          -- new listener, set
          PListener h -> setListener i h obj' k'

  traverseWithKey_ include toInclude


patchChildren
  :: MonadUnliftIO m
#ifndef ghcjs_HOST_OS
  => MonadJSM m
#endif
  => Show a
  => NFData a
  => RawNode -> [ParVNode a] -> [ParVNode a] -> ParDiffT a m [ParVNode a]
patchChildren (RawNode p) [] new = liftJSM $ do
  forM_ new $ \newChild -> do
    RawNode cRaw <- runOnce (getRaw newChild)
    p # "appendChild" $ cRaw
  pure new
patchChildren _ old [] = liftJSM $ do
  doc <- jsg "document"
  tmp <- doc # "createElement" $ "div"
  old' <- traverse (fmap unRawNode . runOnce . getRaw) old
  void (tmp # "replaceChildren" $ old')
  void (tmp # "remove" $ ())
  pure []
patchChildren parent (old:olds) (new:news) =
  (:) <$> patch' parent old new <*> patchChildren parent olds news


patch'
  :: MonadUnliftIO m
#ifndef ghcjs_HOST_OS
  => MonadJSM m
#endif
  => Show a
  => NFData a
  => RawNode -> ParVNode a -> ParVNode a -> ParDiffT a m (ParVNode a)
patch' parent old new = do
  i <- ask
  case (old, new) of

    (ParTextNode raw t', ParTextNode _ t)
      -- text node did not change
      | t == t' -> return old
      -- text node changed
      | otherwise -> liftJSM $ do
        RawNode r <- runOnce raw
        obj' <- makeObject r
        tNew <- valMakeString =<< htmlDecode (toJSString t)
        unsafeSetProp "nodeValue" tNew obj'
        return (ParTextNode raw t)

    -- node may have changed
    (ParNode raw name ps cs, ParNode _ name' ps' cs')
      | name == name' -> do
        raw' <- liftJSM $ do
          RawNode r <- runOnce raw
          obj' <- makeObject r
          managePropertyState i obj' ps ps'
          pure (RawNode r)
        cs'' <- patchChildren raw' cs cs'
        return $ ParNode raw name ps' cs''

    -- node definitely has changed
    _ -> liftJSM $ do
        let RawNode p = parent
        RawNode r <- runOnce $ getRaw old
        RawNode c <- runOnce $ getRaw new
        _ <- p # "replaceChild" $ (c, r)
        return new


{-# SPECIALIZE interpret' :: forall a. NFData a => (JSM ~> JSM) -> Html (ParDiffT a JSM) a -> ParDiffT a JSM (ParVNode a) #-}

interpret'
  :: forall m a
   . MonadJSM m
  => NFData a
  => (m ~> JSM) -> Html (ParDiffT a m) a -> ParDiffT a m (ParVNode a)
interpret' toJSM (Html h') = h' mkNode mkPotato mkText
  where
    mkNode :: Text -> [(Text, Prop (ParDiffT a m) a)] -> [ParDiffT a m (ParVNode a)] -> ParDiffT a m (ParVNode a)
    mkNode name ps cs = do
      cs' <- sequence cs
      i <- ask
      let ps' = toProps ps
      raw <- liftJSM . newOnce $ do
        doc <- jsg "document"
        raw' <- doc # "createElement" $ name
        props toJSM i ps' (RawNode raw')
        forM_ cs' $ \c -> do
          RawNode cRaw <- runOnce (getRaw c)
          raw' # "appendChild" $ cRaw
        return (RawNode raw')

      let p = Props (makeProp toJSM i <$> getProps ps')

      return $ ParNode raw name p cs'

    mkPotato :: JSM (RawNode, STM (Continuation (ParDiffT a m) a)) -> ParDiffT a m (ParVNode a)
    mkPotato mrn = ask >>= \i -> liftJSM $ do
      (rn, stm) <- mrn
      let go = atomically stm >>= writeUpdate i . hoist (toJSM . runParDiff i) >> go
      void $ forkIO go
      fmap ParPotato $ newOnce $ pure rn

    mkText :: Text -> ParDiffT a m (ParVNode a)
    mkText t = liftJSM $ do
      raw <- newOnce $ do
        doc <- jsg "document"
        t' <- valMakeString =<< htmlDecode (toJSString t)
        RawNode <$> (doc # "createTextNode" $ t')
      return $ ParTextNode raw t


instance
  ( MonadUnliftIO m
  , MonadJSM m
  , NFData a
  , Show a ) => Backend (ParDiffT a) m a where
  type VNode (ParDiffT a) m = ParVNode a
  interpret = interpret'
  setup = setup'
  patch parent mOld new = case mOld of
    -- first patch
    Nothing ->
      liftJSM $ do
        let RawNode p = parent
        RawNode c <- runOnce (getRaw new)
        _ <- p # "appendChild" $ c
        return new
    Just old -> patch' parent old new


-- | Get the DOM node produced by 'setup'.
stage :: FromJSVal b => MonadJSM m => ParDiffT a m b
stage = liftJSM $ fromJSValUnchecked =<< jsg "container"
