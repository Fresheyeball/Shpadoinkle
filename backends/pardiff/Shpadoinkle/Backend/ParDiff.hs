{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

#ifndef ghcjs_HOST_OS
{-# LANGUAGE StandaloneDeriving         #-}
#endif


{-|
   This backend is to serve as a cannonical representation of a well
   behaved backend. Defining well behaved in the context of web development
   is rather difficult, and complex.

   The rules of a backend are informal. Roughly, if we give the backend
   some Html, we expect it to update the dom at runtime in the way we expect.

   Since this is cannonical, all other backends are expected to behave
   identically to this one. If differences exist they should be patched,
   so that we retain renderer polymorphism. Such that we can change out
   the renderer of our application, without updating the application logic
   with confidence it will behave as expected.
-}


module Shpadoinkle.Backend.ParDiff
  ( ParDiffT (..)
  , runParDiff
  , stage
  ) where


import           Control.Applicative
import           Control.Compactable
import           Control.Lens
import           Control.Monad.Reader
import           Data.Align
import           Data.Foldable
import           Data.Kind
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Monoid                 ((<>))
import           Data.Once
import           Data.Text
import           Data.These
import           Data.Traversable
import           Data.UUID
import           GHC.Generics
import           Language.Javascript.JSaddle hiding (( # ))
import           NeatInterpolation
import           System.Random
import           UnliftIO

import           Shpadoinkle                 hiding (h, name, props, text)


default (Text)


newtype ParDiffT s model m a = ParDiffT { unParDiff :: ReaderT (s model) m a }
  deriving
  ( Functor
  , Applicative
  , Alternative
  , Monad
  , MonadIO
  , MonadReader (s model)
  , MonadTrans
  )


#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (ParDiffT s model m)
#endif


instance MonadUnliftIO m => MonadUnliftIO (ParDiffT s r m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = ParDiffT . ReaderT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runReaderT r . unParDiff))
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    ParDiffT . ReaderT $ \r ->
    withRunInIO $ \run' ->
    inner (run' . flip runReaderT r . unParDiff)


runParDiff :: t model -> ParDiffT t model m ~> m
runParDiff t (ParDiffT r) = runReaderT r t


data ParVNode :: Type -> Type where
  ParNode     :: Once JSM RawNode -> Text -> Map Text (ParVProp a) -> [ParVNode a] -> ParVNode a
  ParPotato   :: Once JSM RawNode -> ParVNode a
  ParTextNode :: Once JSM RawNode -> Text -> ParVNode a


instance Show (ParVNode a) where
  show = \case
    ParNode _ t ps cs -> "ParNode _ " <> show t <> " " <> show ps <> " " <> show cs
    ParPotato _       -> "ParPotato _"
    ParTextNode _ t   -> "ParTextNode _ " <> show t


data ParVProp a = ParVText Text | ParVListen UUID (RawNode -> RawEvent -> JSM a) | ParVFlag Bool
  deriving (Functor, Generic)


instance Show (ParVProp a) where
  show = \case
    ParVText t     -> "ParVText " <> show t
    ParVListen u _ -> "ParVListen " <> show u <>" _"
    ParVFlag b     -> "ParVFlag " <> show b


props :: Territory s => (m ~> JSM) -> s a -> Map Text (Prop (ParDiffT s a m) a) -> RawNode -> JSM ()
props toJSM i ps (RawNode raw) = do
  raw' <- makeObject raw
  void . traverse (uncurry $ prop toJSM i raw') $ M.toList ps


prop :: Territory s => (m ~> JSM) -> s a -> Object -> Text -> Prop (ParDiffT s a m) a -> JSM ()
prop toJSM i raw k = \case
  PText t     -> setProp' raw k t
  PListener f -> setListener i (\x y -> toJSM . runParDiff i $ f x y) raw k
  PFlag True  -> setProp' raw k =<< toJSVal True
  PFlag False -> return ()


setProp' :: ToJSVal t => Object -> Text -> t -> JSM ()
setProp' raw' k t = do
  let k' = toJSString k
  old <- unsafeGetProp k' raw'
  t' <- toJSVal t
  b <- strictEqual old t'
  if b then return () else unsafeSetProp (toJSString k) t' raw'


setListener :: Territory s => s a -> (RawNode -> RawEvent -> JSM a) -> Object -> Text -> JSM ()
setListener i m o k = do
  elm <- RawNode <$> toJSVal o
  setProp' o ("on" <> k) . fun $ \_ _ -> \case
    e:_ -> writeUpdate i =<< m elm (RawEvent e)
    _ -> return ()


getRaw :: ParVNode a -> Once JSM RawNode
getRaw = \case
  ParNode mk _ _ _ -> mk
  ParPotato mk     -> mk
  ParTextNode mk _ -> mk


setRaw :: Once JSM RawNode -> ParVNode a -> ParVNode a
setRaw r = \case
  ParNode _ a b c -> ParNode r a b c
  ParPotato _     -> ParPotato r
  ParTextNode _ a -> ParTextNode r a


appendChild :: RawNode -> ParVNode a -> JSM (ParVNode a)
appendChild (RawNode raw) pn = do
  let raw' = getRaw pn
  RawNode r <- runOnce raw'
  void $ raw ^. js1 "appendChild" r
  return pn


makeProp :: (m ~> JSM) -> t a -> Prop (ParDiffT t a m) a -> JSM (ParVProp a)
makeProp toJSM i = \case
  PText t     -> return $ ParVText t
  PListener m -> do
    u <- liftIO randomIO
    return . ParVListen u $ \x y -> toJSM . runParDiff i $ m x y
  PFlag b     -> return $ ParVFlag b


setup' :: MonadJSM m => JSM () -> ParDiffT s a m ()
setup' cb = liftJSM $ do
  void $ eval @Text [text|
      window.deleteProp = (k, obj) => {
        delete obj[k]
      }
      window.container = document.createElement('div')
      document.body.appendChild(container)
    |]
  liftJSM cb


voidJSM :: MonadJSM m => JSM a -> m ()
voidJSM = void . liftJSM


setFlag :: MonadJSM m => Object -> Text -> Bool -> m ()
setFlag obj' k b = if b then
    voidJSM $ setProp' obj' k =<< toJSVal True
  else case k of
    "checked" -> voidJSM $ setProp' obj' k =<< toJSVal False
    _         -> voidJSM $ jsg2 "deleteProp" (toJSString k) obj'


managePropertyState :: Territory s => MonadJSM m => s a -> Object -> Map Text (ParVProp a) -> Map Text (ParVProp a) -> m ()
managePropertyState i obj' old new' = void $
  M.toList (align old new') `for` \(k, x) -> case x of
    -- only old had it, delete
    This _                 -> case k of
      "className" -> voidJSM $ obj' ^. js1 "removeAttribute" "class"
      "htmlFor"   -> voidJSM $ obj' ^. js1 "removeAttribute" "for"
      "checked"   -> voidJSM $ setProp' obj' k =<< toJSVal False
      _           -> voidJSM $ jsg2 "deleteProp" (toJSString k) obj'
    -- new text prop, set
    That  (ParVText t)     -> voidJSM $ setProp' obj' k =<< toJSVal t
    -- changed text prop, set
    These (ParVText t)
          (ParVText t')
                | t /= t'  -> voidJSM $ setProp' obj' k =<< toJSVal t'
    -- new flag prop, set
    That  (ParVFlag b)  -> setFlag obj' k b
    -- changed flag prop, set
    These (ParVFlag t)
          (ParVFlag t')
                | t /= t' -> setFlag obj' k t'
    -- new listner, set
    That  (ParVListen _ h) -> voidJSM $ setListener i h obj' k
    -- changed listener, set
    These (ParVListen u _) (ParVListen u' h) | u /= u' -> voidJSM $ setListener i h obj' k
    -- no change, do nothing
    These _ _              -> return ()


patchChildren
  :: MonadUnliftIO m
#ifndef ghcjs_HOST_OS
  => MonadJSM m
#endif
  => Show a
  => Territory s
  => RawNode -> [ParVNode a] -> [ParVNode a] -> ParDiffT s a m [ParVNode a]
patchChildren parent@(RawNode p) old new'' =
  traverseMaybe (\case

    This child -> do
      RawNode c <- lift . liftJSM . runOnce $ getRaw child
      voidJSM $ p ^. js1 "removeChild" c
      return Nothing

    That child -> do
      RawNode c <- lift . liftJSM . runOnce $ getRaw child
      voidJSM $ p ^. js1 "appendChild" c
      return $ Just child

    These old' new' ->
      Just <$> patch' parent (Just old') new'

  ) (align old new'')


patch'
  :: MonadUnliftIO m
#ifndef ghcjs_HOST_OS
  => MonadJSM m
#endif
  => Show a
  => Territory s
  => RawNode -> Maybe (ParVNode a) -> ParVNode a -> ParDiffT s a m (ParVNode a)
patch' parent old new' = do
  i <- ask
  case (old, new') of

    -- text node did not change
    (Just old'@(ParTextNode _ t)
              , ParTextNode _ t')
                         | t == t' -> return old'


    -- text node changed
    (Just (ParTextNode raw _)
         , ParTextNode _ t) -> do

      RawNode r <- liftJSM $ runOnce raw
      obj' <- liftJSM $ makeObject r
      liftJSM $ setProp' obj' "nodeValue" =<< toJSVal t
      return $ setRaw raw new'


    -- node may have changed
    (Just (ParNode raw name ps cs)
         , ParNode _   name' ps' cs')
                 | name == name' -> do

      raw'@(RawNode r) <- liftJSM $ runOnce raw
      obj' <- liftJSM $ makeObject r
      managePropertyState i obj' ps ps'
      cs'' <- patchChildren raw' cs cs'
      return $ ParNode raw name ps' cs''


    -- node definately has changed
    (Just old', _) -> do

      RawNode p <- return parent
      RawNode r <- lift . liftJSM . runOnce $ getRaw old'
      RawNode c <- lift . liftJSM . runOnce $ getRaw new'
      _ <- liftJSM $ p ^. js2 "replaceChild" c r
      return new'


    -- first patch
    (Nothing, _) -> do

      RawNode p <- return parent
      RawNode c <- lift . liftJSM . runOnce $ getRaw new'
      _ <- liftJSM $ p ^. js1 "appendChild" c
      return new'


interpret'
  :: MonadJSM m
  => MonadUnliftIO m
  => Eq a
  => Show a
  => Territory s
  => (m ~> JSM) -> Html (ParDiffT s a m) a -> ParDiffT s a m (ParVNode a)
interpret' toJSM = \case

  TextNode t -> do
    raw <- liftJSM . newOnce $ do
      doc <- jsg "document"
      RawNode <$> doc ^. js1 "createTextNode" t
    return $ ParTextNode raw t

  Potato p -> do
    raw <- liftJSM $ newOnce p
    return $ ParPotato raw

  Node name (M.fromList -> ps) cs -> do
    i <- ask

    let makeNode = do
          doc <- jsg "document"
          elm <- RawNode <$> doc ^. js1 "createElement" name
          props toJSM i ps elm
          return elm

    cs' <- traverse (interpret toJSM) cs
    raw <- liftJSM . newOnce $ do
      node <- makeNode
      traverse_ (appendChild node) cs'
      return node

    p <- liftJSM $ makeProp toJSM i `traverse` ps

    return $ ParNode raw name p cs'


instance
  ( MonadUnliftIO m
  , MonadJSM m
  , Eq a
  , Show a
  , Territory t ) => Backend (ParDiffT t a) m a where
  type VNode (ParDiffT t a) m = ParVNode a
  interpret = interpret'
  setup = setup'
  patch = patch'


stage :: FromJSVal b => MonadJSM m => ParDiffT s a m b
stage = liftJSM $ fromJSValUnchecked =<< jsg "container"
