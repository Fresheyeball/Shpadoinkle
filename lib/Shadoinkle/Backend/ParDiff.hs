{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Shpadoinkle.Backend.ParDiff
  ( ParDiffT (..)
  , runParDiffNT
  , stage
  ) where


import           Control.Lens
import           Control.Monad.Reader
import           Control.Natural
import           Data.Align
import qualified Data.Map                    as M
import           Data.Once
import           Data.These
import           Data.UUID
import           Language.Javascript.JSaddle hiding (( # ))
import           NeatInterpolation
import           System.Random

import           Shpadoinkle


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
  )


deriving instance MonadJSM m => MonadJSM (ParDiffT model m)
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


runParDiffNT :: TVar model -> ParDiffT model m :~> m
runParDiffNT t = NT $ \(ParDiffT r) -> runReaderT r t


data ParVNode :: Type -> Type where
  ParNode     :: Once JSM RawNode -> Text -> Map Text (ParVProp a) -> [ParVNode a] -> ParVNode a
  ParTextNode :: Once JSM RawNode -> Text -> ParVNode a


instance Show (ParVNode a) where
  show = \case
    ParNode _ t ps cs -> "ParNode _ " <> show t <> " " <> show ps <> " " <> show cs
    ParTextNode _ t   -> "ParTextNode _ " <> show t


data ParVProp a = ParVText Text | ParVListen UUID (JSM a) | ParVFlag
  deriving (Functor, Generic)


instance Show (ParVProp a) where
  show = \case
    ParVText t     -> "ParVText " <> show t
    ParVListen u _ -> "ParVListen " <> show u <>" _"
    ParVFlag       -> "ParVFlag"


props :: (m :~> JSM) -> TVar a -> Map Text (Prop (ParDiffT a m) a) -> RawNode -> JSM ()
props toJSM i ps (RawNode raw) = do
  raw' <- makeObject raw
  void . traverse (uncurry $ prop toJSM i raw') $ M.toList ps


prop :: (m :~> JSM) -> TVar a -> Object -> Text -> Prop (ParDiffT a m) a -> JSM ()
prop toJSM i raw k = \case
  PText t -> setProp' raw k t

  PListener f ->
    setListener i (unwrapNT (toJSM . runParDiffNT i) f) raw k

  PFlag -> setProp' raw k =<< toJSVal True


setProp' :: ToJSVal t => Object -> Text -> t -> JSM ()
setProp' raw' k t = do
   t' <- toJSVal t
   unsafeSetProp (JSString k) t' raw'


setListener :: TVar a -> JSM a -> Object -> Text -> JSM ()
setListener i m o k = setProp' o ("on" <> k) . fun $ \_ _ _ ->
  atomically . writeTVar i =<< m


getRaw :: ParVNode a -> Once JSM RawNode
getRaw = \case
  ParNode mk _ _ _ -> mk
  ParTextNode mk _ -> mk


setRaw :: Once JSM RawNode -> ParVNode a -> ParVNode a
setRaw r = \case
  ParNode _ a b c -> ParNode r a b c
  ParTextNode _ a -> ParTextNode r a


appendChild :: RawNode -> ParVNode a -> JSM (ParVNode a)
appendChild (RawNode raw) pn = do
  let raw' = getRaw pn
  RawNode r <- runOnce raw'
  void $ raw ^. js1 "appendChild" r
  return pn


makeProp :: (m :~> JSM) -> TVar a -> Prop (ParDiffT a m) a -> JSM (ParVProp a)
makeProp toJSM i = \case
  PText t     -> return $ ParVText t
  PListener m -> do
    u <- liftIO randomIO
    return . ParVListen u $ unwrapNT (toJSM . runParDiffNT i) m
  PFlag       -> return ParVFlag


setup' :: MonadJSM m => JSM () -> ParDiffT a m ()
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


managePropertyState :: MonadJSM m => TVar a -> Object -> Map Text (ParVProp a) -> Map Text (ParVProp a) -> m ()
managePropertyState i obj' old new' = void $
  M.toList (align old new') `for` \(k, x) -> case x of
    -- only old had it, delete
    This _                 -> voidJSM $ jsg2 "deleteProp" (JSString k) obj'
    -- new text prop, set
    That  (ParVText t)     -> voidJSM $ setProp' obj' k =<< toJSVal t
    -- changed text prop, set
    These (ParVText t)
          (ParVText t')
                | t /= t'  -> voidJSM $ setProp' obj' k =<< toJSVal t
    -- new flag prop, set
    That   ParVFlag        -> voidJSM $ setProp' obj' k =<< toJSVal True
    -- new listner, set
    That  (ParVListen _ h) -> voidJSM $ setListener i h obj' k
    -- changed listener, set
    These (ParVListen u _) (ParVListen u' h) | u /= u' -> voidJSM $ setListener i h obj' k
    -- no change, do nothing
    These _ _              -> return ()


patchChildren
  :: MonadUnliftIO m
  => MonadJSM m
  => Show a
  => RawNode -> [ParVNode a] -> [ParVNode a] -> ParDiffT a m [ParVNode a]
patchChildren parent@(RawNode p) old new'' =
  compact <$> mapConcurrently (\case

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
  => MonadJSM m
  => Show a
  => RawNode -> Maybe (ParVNode a) -> ParVNode a -> ParDiffT a m (ParVNode a)
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
      liftJSM $ p ^. js2 "replaceChild" c r
      return $ setRaw (getRaw old') new'


    -- first patch
    (Nothing, _) -> do

      RawNode p <- return parent
      RawNode c <- lift . liftJSM . runOnce $ getRaw new'
      liftJSM $ p ^. js1 "appendChild" c
      return new'


interpret'
  :: MonadJSM m
  => MonadUnliftIO m
  => Eq a
  => Show a
  => (m :~> JSM) -> Html (ParDiffT a m) a -> ParDiffT a m (ParVNode a)
interpret' toJSM = \case

  TextNode t -> do
    raw <- liftJSM . newOnce $ do
      doc <- jsg "document"
      RawNode <$> doc ^. js1 "createTextNode" t
    return $ ParTextNode raw t

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


instance (MonadUnliftIO m, MonadJSM m, Eq a, Show a) => Shpadoinkle (ParDiffT a) m a where
  type VNode (ParDiffT a) m = ParVNode a
  interpret = interpret'
  setup = setup'
  patch = patch'


stage :: FromJSVal b => MonadJSM m => ParDiffT a m b
stage = liftJSM $ fromJSValUnchecked =<< jsg "container"
