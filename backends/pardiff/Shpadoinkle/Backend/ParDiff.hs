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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
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
import           Control.Monad               (forM_, void, when, (<=<))
import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Cont          (MonadCont)
import           Control.Monad.Except        (MonadError)
import           Control.Monad.RWS           (MonadRWS)
import           Control.Monad.Reader        (MonadIO, MonadReader (ask, local),
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
import           Data.Text                   (Text)
import           Shpadoinkle.JSFFI           (JSElement, JSObject, JSString,
                                              fromJSValUnsafe, getGlobal,
                                              getProp, global, jsFalse, jsTrue,
                                              liftJSM, mkFun', purely,
                                              setInnerHTML, setProp, toJSString,
                                              toJSVal, (#))
#ifdef ghcjs_HOST_OS
import           Shpadoinkle.JSFFI           (toJSObject)
#endif
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
instance MonadReader r m => MonadReader r (ParDiffT model m) where
  ask = ParDiffT (ReaderT (const ask))
  local f (ParDiffT (ReaderT g)) = ParDiffT (ReaderT (local f . g))
instance MonadRWS r w s m => MonadRWS r w s (ParDiffT model m)

askModel :: Monad m => ParDiffT model m (TVar model)
askModel = ParDiffT ask



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


traverseWithKey_ :: Applicative t => (k -> a -> t ()) -> Map k a -> t ()
traverseWithKey_ f = go
  where
    go Tip             = pure ()
    go (Bin 1 k v _ _) = f k v
    go (Bin _ k v l r) = go l *> f k v *> go r
{-# INLINE traverseWithKey_ #-}
{-# SPECIALIZE traverseWithKey_ :: (k -> a -> JSM ()) -> Map k a -> JSM () #-}


data ParVNode :: Type -> Type where
  ParNode     :: Once JSM RawNode -> {-# UNPACK #-} !Text -> ParVProps a -> [ParVNode a] -> ParVNode a
  ParPotato   :: Once JSM RawNode -> ParVNode a
  ParTextNode :: Once JSM RawNode -> {-# UNPACK #-} !Text -> ParVNode a

type ParVProps = Props JSM

type ParVProp = Prop JSM


-- | Interpret uninterpreted props into the JavaScript DOM node
props :: Monad m
      => NFData a
      => (m ~> JSM)
      -> TVar a -- ^ Model
      -> Props (ParDiffT a m) a -- ^ Uninterpreted Props
      -> RawNode -- ^ DOM Node
      -> JSM ()
props toJSM i (Props ps) (RawNode raw) = do
  let raw' = fromJSValUnsafe raw
  traverseWithKey_ (prop toJSM i raw') ps


-- | Interpret a single prop into the JavaScript DOM node
prop :: Monad m
     => NFData a
     => (m ~> JSM)
     -> TVar a -- ^ Model
     -> JSObject -- ^ DOM Node
     -> Text -- ^ Property Key
     -> Prop (ParDiffT a m) a -- ^ Uninterpreted Property
     -> JSM ()
prop toJSM i raw k = \case
  PData d     -> setProp k' d raw
  PText t     -> do
    let t' = purely toJSVal t
    setProp k' t' raw
  PPotato p   -> setProptado i (fmap (fmap (hoist (toJSM . runParDiff i))) . p) raw
  PListener f -> setListener i (\x y -> hoist (toJSM . runParDiff i) <$> f x y) raw k'
  PFlag True  -> setProp k' (purely toJSVal jsTrue) raw
  PFlag False -> return ()
  where
    k' = purely toJSString $ k


-- | Evaluates the proptato and loops the 'STM' continuation in a separate thread
setProptado :: NFData a
            => TVar a -- ^ Model
            -> (RawNode -> JSM (STM (Continuation JSM a))) -- ^ Proptato
            -> JSObject -- ^ DOM Node
            -> JSM ()
setProptado i f o = do
  elm <- RawNode <$> toJSVal o
  stm <- f elm
  let go = atomically stm >>= writeUpdate i >> go
  void $ forkIO go


-- | Assigns an event handler to the DOM node
setListener :: NFData a
            => TVar a -- ^ Model
            -> (RawNode -> RawEvent -> JSM (Continuation JSM a)) -- ^ Event Handler
            -> JSObject -- ^ DOM Node
            -> JSString -- ^ Event Name
            -> JSM ()
setListener i m o k = do
  elm <- RawNode <$> toJSVal o
  f <- toJSVal <=< mkFun' $ \case
    e:_ -> do
      x <- m elm (RawEvent e)
      writeUpdate i x
    _ -> return ()
  setProp ("on" <> k) f o


-- | Gets the 'RawNode' from a virtual node, whether evaluated or not
getRaw :: ParVNode a -> Once JSM RawNode
getRaw = \case
  ParNode mk _ _ _ -> mk
  ParPotato mk     -> mk
  ParTextNode mk _ -> mk


-- | Hoists the 'ParDiffT' monad into 'JSM' for props
makeProp :: Monad m
         => (m ~> JSM) -- ^ Natural Transformation
         -> TVar a -- ^ Model
         -> Prop (ParDiffT a m) a -- ^ Unevaluated Property
         -> ParVProp a
makeProp toJSM i = \case
  PText t     -> PText t
  PData t     -> PData t
  PPotato p   -> PPotato $ fmap (fmap (hoist (toJSM . runParDiff i))) . p
  PListener m -> PListener $ \x y -> hoist (toJSM . runParDiff i) <$> m x y
  PFlag b     -> PFlag b


#ifndef ghcjs_HOST_OS
deleteProp :: JSString -> JSObject -> JSM ()
deleteProp _ _ = pure () -- can't delete properties from object in GHC
#else
foreign import javascript unsafe
  "delete $2[$1];"
  deleteProp :: JSString -> JSObject -> JSM ()
#endif


-- | Modify a DOM node in accordance with new properties, and properties to remove
managePropertyState :: NFData a
                    => TVar a -- ^ Model
                    -> JSObject -- ^ DOM Node
                    -> ParVProps a -- ^ Old Props
                    -> ParVProps a -- ^ New Props
                    -> JSM ()
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
    (setProp "checked" (purely toJSVal jsFalse) obj')

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
        "checked"   -> setProp (purely toJSString $ k) (purely toJSVal jsFalse) obj'
        "disabled"  -> void $ obj' # "removeAttribute" $ "disabled"
        _           -> deleteProp (purely toJSString $ k) obj'

  traverseWithKey_ remove toRemove

  let include k v =
        let k' = purely toJSString $ k
        in  case v of
          PPotato p -> void . p . RawNode =<< toJSVal obj' -- FIXME why throw away continuation...???
          PData j -> setProp k' j obj'
          -- new text prop, set
          PText t -> do
            let t' = purely toJSVal t
            setProp k' t' obj'
          -- new flag prop, set
          PFlag b
            | b -> setProp k' (purely toJSVal jsTrue) obj'
            | otherwise -> case k of
              "checked"  -> setProp k' (purely toJSVal jsFalse) obj'
              "disabled" -> void (obj' # "removeAttribute" $ "disabled")
              _          -> deleteProp k' obj'
          -- new listener, set
          PListener h -> setListener i h obj' k'

  traverseWithKey_ include toInclude


-- | Patch sets of virtual nodes (children) together
patchChildren
  :: MonadUnliftIO m
  => Show a
  => NFData a
  => RawNode -> [ParVNode a] -> [ParVNode a] -> ParDiffT a m [ParVNode a]
patchChildren (RawNode p) [] new = liftJSM $ do
  forM_ new $ \newChild -> do
    RawNode cRaw <- runOnce (getRaw newChild)
    fromJSValUnsafe @JSObject p # "appendChild" $ cRaw
  pure new
patchChildren _ old [] = liftJSM $ do
  doc <- getGlobal "document"
  tmp <- fromJSValUnsafe @JSObject doc # "createElement" $ "div"
  old' <- traverse (fmap unRawNode . runOnce . getRaw) old
  void (fromJSValUnsafe @JSObject tmp # "replaceChildren" $ old')
  void (fromJSValUnsafe @JSObject tmp # "remove" $ ())
  pure []
patchChildren parent (old:olds) (new:news) =
  (:) <$> patch' parent old new <*> patchChildren parent olds news


-- | Patch a single node together, recursing through their potential children
patch'
  :: MonadUnliftIO m
  => Show a
  => NFData a
  => RawNode -> ParVNode a -> ParVNode a -> ParDiffT a m (ParVNode a)
patch' parent old new = do
  i <- askModel
  case (old, new) of

    (ParTextNode raw t', ParTextNode _ t)
      -- text node did not change
      | t == t' -> return old
      -- text node changed
      | otherwise -> liftJSM $ do
        RawNode r <- runOnce raw
        let obj' :: JSObject = fromJSValUnsafe r
        tNew <- fmap (purely toJSVal) . htmlDecode . purely toJSString $ t
        setProp "nodeValue" tNew obj'
        return (ParTextNode raw t)

    -- node may have changed
    (ParNode raw name ps cs, ParNode _ name' ps' cs')
      | name == name' -> do
        raw' <- liftJSM $ do
          RawNode r <- runOnce raw
          let obj' = fromJSValUnsafe r
          managePropertyState i obj' ps ps'
          pure (RawNode r)
        cs'' <- patchChildren raw' cs cs'
        return $ ParNode raw name ps' cs''

    -- node definitely has changed
    _ -> liftJSM $ do
        let p :: JSObject = fromJSValUnsafe . unRawNode $ parent
        RawNode r <- runOnce $ getRaw old
        RawNode c <- runOnce $ getRaw new
        _ <- p # "replaceChild" $ (c, r)
        return new


-- | Generate virtual nodes to be patched against
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
      i <- askModel
      let ps' = toProps ps
      raw <- liftJSM . newOnce $ do
        doc <- getGlobal "document"
        raw' <- fromJSValUnsafe @JSObject doc # "createElement" $ name
        props toJSM i ps' (RawNode raw')
        forM_ cs' $ \c -> do
          RawNode cRaw <- runOnce (getRaw c)
          fromJSValUnsafe @JSObject raw' # "appendChild" $ cRaw
        return (RawNode raw')

      let p = Props (makeProp toJSM i <$> getProps ps')

      return $ ParNode raw name p cs'

    mkPotato :: JSM (RawNode, STM (Continuation (ParDiffT a m) a)) -> ParDiffT a m (ParVNode a)
    mkPotato mrn = askModel >>= \i -> liftJSM $ do
      (rn, stm) <- mrn
      let go = atomically stm >>= writeUpdate i . hoist (toJSM . runParDiff i) >> go
      void $ forkIO go
      fmap ParPotato $ newOnce $ pure rn

    mkText :: Text -> ParDiffT a m (ParVNode a)
    mkText t = liftJSM $ do
      raw <- newOnce $ do
        doc :: JSObject <- fromJSValUnsafe <$> getGlobal "document"
        t' <- fmap (purely toJSVal) . htmlDecode . purely toJSString $ t
        RawNode <$> (doc # "createTextNode" $ t')
      return $ ParTextNode raw t


instance
  ( MonadUnliftIO m
  , MonadJSM m
  , NFData a
  , Show a ) => Backend (ParDiffT a) m a where
  type VNode (ParDiffT a) m = ParVNode a
  interpret = interpret'
  setup = id
  patch parent mOld new = case mOld of
    -- first patch
    Nothing ->
      liftJSM $ do
        let p :: JSObject = fromJSValUnsafe . unRawNode $ parent
        RawNode c <- runOnce (getRaw new)
        _ <- p # "appendChild" $ c
        return new
    Just old -> patch' parent old new


-- | Get the @<body>@ DOM node after emptying it.
stage :: MonadJSM m => ParDiffT a m RawNode
stage = liftJSM $ do
  body <- do
    document <- fromJSValUnsafe @JSObject <$> getProp "document" global
    fromJSValUnsafe @JSElement <$> getProp "body" document
  setInnerHTML "" body
  RawNode <$> toJSVal body
{-# SPECIALIZE stage :: ParDiffT a JSM RawNode #-}
