{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}


{-|
  Shpadoinkle Continuation is the abstract structure of Shpadoinkle's event handling system.
  It allows for asynchronous effects in event handlers by providing a model for atomic updates
  of application state.
-}


module Shpadoinkle.Continuation (
  -- * The Continuation Type
  Continuation (..)
  , runContinuation
  , done, pur, impur, kleisli, causes, causedBy, merge, contIso
  -- * The Class
  , Continuous (..)
  -- ** Hoist
  , hoist
  -- * Forgetting
  , voidC', voidC, forgetC
  -- * Lifts
  , liftC', liftCMay', liftC, liftCMay
  -- * Utilities
  -- ** Product
  , leftC', leftC, rightC', rightC
  -- ** Coproduct
  , eitherC', eitherC
  -- ** Maybe
  , maybeC', maybeC, comaybe, comaybeC', comaybeC
  -- * Updates
  , writeUpdate, shouldUpdate, constUpdate
  -- * Monad Transformer
  , ContinuationT (..), voidRunContinuationT, kleisliT, commit
  -- * Re-exports
  , module Control.DeepSeq
  ) where


import           Control.Arrow                           (first)
import qualified Control.Categorical.Functor             as F
import           Control.DeepSeq                         (NFData (..), force)
import           Control.Monad                           (void)
import           Control.Monad.Trans.Class               (MonadTrans (..))
import           Control.PseudoInverseCategory           (EndoIso (..))
import           Data.Maybe                              (fromMaybe)
import           GHC.Conc                                (retry)
import           GHCJS.DOM                               (currentWindowUnchecked)
import           GHCJS.DOM.RequestAnimationFrameCallback (newRequestAnimationFrameCallback)
import           GHCJS.DOM.Window                        (Window,
                                                          cancelAnimationFrame,
                                                          requestAnimationFrame)
import           Language.Javascript.JSaddle             (MonadJSM)
import           UnliftIO                                (MonadUnliftIO, TVar,
                                                          UnliftIO, askUnliftIO,
                                                          atomically, liftIO,
                                                          newTVarIO, readTVar,
                                                          readTVarIO, unliftIO,
                                                          writeTVar)
import           UnliftIO.Concurrent                     (forkIO)


-- | A Continuation builds up an
--   atomic state update incrementally in a series of stages. For each stage we perform
--   a monadic IO computation and we may get a pure state updating function. When
--   all of the stages have been executed we are left with a composition of the resulting
--   pure state updating functions, and this composition is applied atomically to the state.
--
--   Additionally, a Continuation stage may feature a Rollback action which cancels all state
--   updates generated so far but allows for further state updates to be generated based on
--   further monadic IO computation.
--
--   The functions generating each stage of the Continuation
--   are called with states which reflect the current state of the app, with all
--   the pure state updating functions generated so far having been
--   applied to it, so that each stage "sees" both the current state
--   (even if it changed since the start of computing the Continuation), and the updates made
--   so far, although those updates are not committed to the real state until the Continuation
--   finishes and they are all done atomically together.
data Continuation m a = Continuation (a -> a, a -> m (Continuation m a))
                      | Rollback (Continuation m a)
                      | Merge (Continuation m a)
                      | Pure (a -> a)



-- | A pure state updating function can be turned into a Continuation. This function
--   is here so that users of the Continuation API can do basic things without needing
--   to depend on the internal structure of the type.
pur :: (a -> a) -> Continuation m a
pur = Pure


-- | A Continuation which doesn't touch the state and doesn't have any side effects
done :: Continuation m a
done = pur id


-- | A monadic computation of a pure state updating function can be turned into a Continuation.
impur :: Applicative m => m (a -> a) -> Continuation m a
impur m = kleisli . const $ (\f -> Continuation (f, const (pure done))) <$> m


-- | This turns a Kleisli arrow for computing a Continuation into the Continuation which
--   reads the state, runs the monadic computation specified by the arrow on that state,
--   and runs the resulting Continuation.
kleisli :: (a -> m (Continuation m a)) -> Continuation m a
kleisli = Continuation . (id,)


-- | A monadic computation can be turned into a Continuation which does not touch the state.
causes :: Applicative m => m () -> Continuation m a
causes m = impur (id <$ m)


causedBy :: m (Continuation m a) -> Continuation m a
causedBy = Continuation . (id,) . const


-- | A continuation can be forced to write its changes midflight.
merge :: Continuation m a -> Continuation m a
merge = Merge


-- | 'runContinuation' takes a 'Continuation' and a state value and runs the whole Continuation
--   as if the real state was frozen at the value given to 'runContinuation'. It performs all the
--   IO actions in the stages of the Continuation and returns a pure state updating function
--   which is the composition of all the pure state updating functions generated by the
--   non-rolled-back stages of the Continuation. If you are trying to update a 'Continuous'
--   territory, then you should probably be using 'writeUpdate' instead of 'runContinuation',
--   because 'writeUpdate' will allow each stage of the Continuation to see any extant updates
--   made to the territory after the Continuation started running.
runContinuation :: Monad m => Continuation m a -> a -> m (a -> a)
runContinuation = runContinuation' id


runContinuation' :: Monad m => (a -> a) -> Continuation m a -> a -> m (a -> a)
runContinuation' f (Continuation (g, h)) x = do
  i <- h (f x)
  runContinuation' (g.f) i x
runContinuation' _ (Rollback f) x = runContinuation' id f x
runContinuation' f (Merge g) x = runContinuation' f g x
runContinuation' f (Pure g) _ = return (g.f)


-- | @f@ is a Functor to Hask from the category where the objects are
--   Continuation types and the morphisms are functions.
class Continuous f where
  mapC :: (Continuation m a -> Continuation m b) -> f m a -> f m b


instance Continuous Continuation where
  mapC = id


-- | Given a natural transformation, change a Continuation's underlying functor.
hoist :: Functor m => (forall b. m b -> n b) -> Continuation m a -> Continuation n a
hoist _ (Pure f)              = Pure f
hoist f (Rollback r)          = Rollback (hoist f r)
hoist f (Merge g)             = Merge (hoist f g)
hoist f (Continuation (g, h)) = Continuation . (g,) $ \x -> f $ hoist f <$> h x


-- | Apply a lens inside a Continuation to change the Continuation's type.
liftC' :: Functor m => (a -> b -> b) -> (b -> a) -> Continuation m a -> Continuation m b
liftC' f g (Pure h)              = Pure (\x -> f (h (g x)) x)
liftC' f g (Rollback r)          = Rollback (liftC' f g r)
liftC' f g (Merge h)             = Merge (liftC' f g h)
liftC' f g (Continuation (h, i)) = Continuation (\x -> f (h (g x)) x, \x -> liftC' f g <$> i (g x))


-- | Apply a traversal inside a Continuation to change the Continuation's type.
liftCMay' :: Applicative m => (a -> b -> b) -> (b -> Maybe a) -> Continuation m a -> Continuation m b
liftCMay' f g (Pure h)     = Pure $ \x -> maybe x (flip f x . h) $ g x
liftCMay' f g (Rollback r) = Rollback (liftCMay' f g r)
liftCMay' f g (Merge h)    = Merge (liftCMay' f g h)
liftCMay' f g (Continuation (h, i)) =
  Continuation (\x -> maybe x (flip f x . h) $ g x, maybe (pure done) (fmap (liftCMay' f g) . i) . g)


-- | Given a lens, change the value type of @f@ by applying the lens in the Continuations inside @f@.
liftC :: Functor m => Continuous f => (a -> b -> b) -> (b -> a) -> f m a -> f m b
liftC f g = mapC (liftC' f g)


-- | Given a traversal, change the value of @f@ by apply the traversal in the Continuations inside @f@.
liftCMay :: Applicative m => Continuous f => (a -> b -> b) -> (b -> Maybe a) -> f m a -> f m b
liftCMay f g = mapC (liftCMay' f g)


-- | Change a void continuation into any other type of Continuation.
voidC' :: Monad m => Continuation m () -> Continuation m a
voidC' f = Continuation . (id,) $ \_ -> do
  _ <- runContinuation f ()
  return done


-- | Change the type of the f-embedded void Continuations into any other type of Continuation.
voidC :: Monad m => Continuous f => f m () -> f m a
voidC = mapC voidC'


-- | Forget about the Continuations.
forgetC :: Continuous f => f m a -> f m b
forgetC = mapC (const done)


--- | Change the type of a Continuation by applying it to the left coordinate of a tuple.
leftC' :: Functor m => Continuation m a -> Continuation m (a,b)
leftC' = liftC' (\x (_,y) -> (x,y)) fst


-- | Change the type of @f@ by applying the Continuations inside @f@ to the left coordinate of a tuple.
leftC :: Functor m => Continuous f => f m a -> f m (a,b)
leftC = mapC leftC'


-- | Change the type of a Continuation by applying it to the right coordinate of a tuple.
rightC' :: Functor m => Continuation m b -> Continuation m (a,b)
rightC' = liftC' (\y (x,_) -> (x,y)) snd


-- | Change the value type of @f@ by applying the Continuations inside @f@ to the right coordinate of a tuple.
rightC :: Functor m => Continuous f => f m b -> f m (a,b)
rightC = mapC rightC'


-- | Transform a Continuation to work on 'Maybe's. If it encounters 'Nothing', then it cancels itself.
maybeC' :: Applicative m => Continuation m a -> Continuation m (Maybe a)
maybeC' (Pure f)              = Pure (fmap f)
maybeC' (Rollback r)          = Rollback (maybeC' r)
maybeC' (Merge f)             = Merge (maybeC' f)
maybeC' (Continuation (f, g)) = Continuation . (fmap f,) $
  \case
    Just x  -> maybeC' <$> g x
    Nothing -> pure (Rollback done)


-- | Change the value type of @f@ by transforming the Continuations inside @f@ to work on 'Maybe's using maybeC'.
maybeC :: Applicative m => Continuous f => f m a -> f m (Maybe a)
maybeC = mapC maybeC'


-- | Turn a @Maybe a@ updating function into an @a@ updating function which acts as
--   the identity function when the input function outputs 'Nothing'.
comaybe :: (Maybe a -> Maybe a) -> (a -> a)
comaybe f x = fromMaybe x . f $ Just x


-- | Change the type of a Maybe-valued Continuation into the Maybe-wrapped type.
--   The resulting Continuation acts like the input Continuation except that
--   when the input Continuation would replace the current value with 'Nothing',
--   instead the current value is retained.
comaybeC' :: Functor m => Continuation m (Maybe a) -> Continuation m a
comaybeC' (Pure f)             = Pure (comaybe f)
comaybeC' (Rollback r)         = Rollback (comaybeC' r)
comaybeC' (Merge f)            = Merge (comaybeC' f)
comaybeC' (Continuation (f,g)) = Continuation (comaybe f, fmap comaybeC' . g . Just)


-- | Transform the Continuations inside @f@ using comaybeC'.
comaybeC :: Functor m => Continuous f => f m (Maybe a) -> f m a
comaybeC = mapC comaybeC'


-- Just define these rather than introducing another dependency even though they are in either
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)  = Left (f x)
mapLeft _ (Right x) = Right x


mapRight :: (b -> c) -> Either a b -> Either a c
mapRight _ (Left x)  = Left x
mapRight f (Right x) = Right (f x)


-- | Combine Continuations heterogeneously into coproduct Continuations.
--   The first value the Continuation sees determines which of the
--   two input Continuation branches it follows. If the coproduct Continuation
--   sees the state change to a different Either-branch, then it cancels itself.
--   If the state is in a different Either-branch when the Continuation
--   completes than it was when the Continuation started, then the
--   coproduct Continuation will have no effect on the state.
eitherC' :: Applicative m => Continuation m a -> Continuation m b -> Continuation m (Either a b)
eitherC' f g = Continuation . (id,) $ \case
  Left x -> case f of
    Pure h -> pure (Pure (mapLeft h))
    Rollback r -> pure . Rollback $ eitherC' r done
    Merge h -> pure . Merge $ eitherC' h done
    Continuation (h, i) ->
      (\j -> Continuation (mapLeft h, const . pure $ eitherC' j (Rollback done)))
        <$> i x
  Right x -> case g of
    Pure h -> pure (Pure (mapRight h))
    Rollback r -> pure . Rollback $ eitherC' done r
    Merge h -> pure . Merge $ eitherC' done h
    Continuation (h, i) ->
      (\j -> Continuation (mapRight h, const . pure $ eitherC' (Rollback done) j))
        <$> i x


-- | Create a structure containing coproduct Continuations using two case
--   alternatives which generate structures containing Continuations of
--   the types inside the coproduct. The Continuations in the resulting
--   structure will only have effect on the state while it is in the branch
--   of the coproduct selected by the input value used to create the structure.
eitherC :: Applicative m => Continuous f => (a -> f m a) -> (b -> f m b) -> Either a b -> f m (Either a b)
eitherC l _ (Left x)  = mapC (\c -> eitherC' c (pur id)) (l x)
eitherC _ r (Right x) = mapC (eitherC' (pur id)) (r x)


-- | Transform the type of a Continuation using an isomorphism.
contIso :: Functor m => (a -> b) -> (b -> a) -> Continuation m a -> Continuation m b
contIso f g (Continuation (h, i)) = Continuation (f.h.g, fmap (contIso f g) . i . g)
contIso f g (Rollback h) = Rollback (contIso f g h)
contIso f g (Merge h)    = Merge (contIso f g h)
contIso f g (Pure h)     = Pure (f.h.g)


-- | @Continuation m@ is a Functor in the EndoIso category (where the objects
--   are types and the morphisms are EndoIsos).
instance Applicative m => F.Functor EndoIso EndoIso (Continuation m) where
  map (EndoIso f g h) =
    EndoIso (Continuation . (f,) . const . pure) (contIso g h) (contIso h g)


-- | You can combine multiple Continuations homogeneously using the 'Monoid' typeclass
--   instance. The resulting Continuation will execute all the subcontinuations in parallel,
--   allowing them to see each other's state updates and roll back each other's updates,
--   applying all of the unmerged updates generated by all the subcontinuations atomically once
--   all of them are done. A merge in any one of the branches will cause all of
--   the changes that branch can see to be merged.
instance Applicative m => Semigroup (Continuation m a) where
  (Continuation (f, g)) <> (Continuation (h, i)) =
    Continuation (f.h, \x -> (<>) <$> g x <*> i x)
  (Continuation (f, g)) <> (Rollback h) =
    Rollback (Continuation (f, fmap (<> h) . g))
  (Rollback h) <> (Continuation (_, g)) =
    Rollback (Continuation (id, fmap (h <>) . g))
  (Rollback f) <> (Rollback g) = Rollback (f <> g)
  (Pure f) <> (Pure g) = Pure (f.g)
  (Pure f) <> (Continuation (g,h)) = Continuation (f.g,h)
  (Continuation (f,g)) <> (Pure h) = Continuation (f.h,g)
  (Pure f) <> (Rollback g) = Continuation (f, const (pure (Rollback g)))
  (Rollback f) <> (Pure _) = Rollback f
  (Merge f) <> g = Merge (f <> g)
  f <> (Merge g) = Merge (f <> g)


-- | Since combining Continuations homogeneously is an associative operation,
--   and this operation has a unit element (done), Continuations are a 'Monoid'.
instance Applicative m => Monoid (Continuation m a) where
  mempty = done


writeUpdate' :: MonadUnliftIO m => NFData a => (a -> a) -> TVar a -> (a -> m (Continuation m a)) -> m ()
writeUpdate' h model f = do
  i <- readTVarIO model
  m <- f (h i)
  case m of
    Continuation (g,gs) -> writeUpdate' (g . h) model gs
    Pure g -> atomically (writeTVar model . g . h =<< readTVar model)
    Merge g -> do
      atomically $ writeTVar model . h =<< readTVar model
      writeUpdate' id model (const (return g))
    Rollback gs -> writeUpdate' id model (const (return gs))


-- | Run a Continuation on a state variable. This may update the state.
--   This is a synchronous, non-blocking operation for pure updates,
--   and an asynchronous, non-blocking operation for impure updates.
writeUpdate :: MonadUnliftIO m => NFData a => TVar a -> Continuation m a -> m ()
writeUpdate model = \case
  Continuation (f,g) -> void . forkIO $ writeUpdate' f model g
  Pure f             -> atomically (writeTVar model . f =<< readTVar model)
  Merge f            -> writeUpdate model f
  Rollback f         -> writeUpdate model f


-- | Execute a fold by watching a state variable and executing the next
--   step of the fold each time it changes.
shouldUpdate :: forall a b m. MonadJSM m => MonadUnliftIO m => Eq a => (b -> a -> m b) -> b -> TVar a -> m ()
shouldUpdate sun prev currentModel = do
  -- get the current state of the model
  sampleModel   :: a          <- readTVarIO currentModel
  -- duplicate that state so we can compare if the model changes
  previousModel :: TVar a     <- newTVarIO sampleModel
  -- store the accumulating value in a TVar so we can control when it updates
  currentState  :: TVar b     <- newTVarIO prev
  -- get the window once
  window        :: Window     <- currentWindowUnchecked
  -- get the execution context once
  context       :: UnliftIO m <- askUnliftIO

  let
    go :: Maybe Int -> m ()
    go frameId = do

      -- block if the new model is the same as the old
      newModel <- atomically $ do
        new' <- readTVar currentModel
        old  <- readTVar previousModel
        -- if the new model is different from the old
        -- unblock and write the new model for the next comparision
        if new' == old then retry else new' <$ writeTVar previousModel new'

      -- if we already had something scheduled to run, cancel it
      maybe (pure ()) (cancelAnimationFrame window) frameId

      -- generate a callback for the request animation frame
      callback <- newRequestAnimationFrameCallback $ \_ -> do
        -- get the current state
        x <- readTVarIO currentState
        -- run the action against the current state, and the new model
        y <- liftIO $ unliftIO context $ sun x newModel
        -- write the new state for the next run
        atomically $ writeTVar currentState y
        -- note this means that @newModel@ updates for each call to @go@
        -- but @currentState@ only updates if the frame is actually called

      -- schedule the action to run on the next frame
      frameId' <- requestAnimationFrame window callback

      go (Just frameId')

  () <$ forkIO (go Nothing)


-- | A monad transformer for building up a Continuation in a series of steps in a monadic computation
newtype ContinuationT model m a = ContinuationT
  { runContinuationT :: m (a, Continuation m model) }


-- | This adds the given Continuation to the Continuation being built up in the monadic context
--   where this function is invoked.
commit :: Applicative m => Continuation m model -> ContinuationT model m ()
commit = ContinuationT . pure . ((),)


-- | This turns a monadic computation to build up a Continuation into the Continuation which it
--   represents. The actions inside the monadic computation will be run when the Continuation
--   is run. The return value of the monadic computation will be discarded.
voidRunContinuationT :: Functor m => ContinuationT model m a -> Continuation m model
voidRunContinuationT m = kleisli . const $ snd <$> runContinuationT m


-- | This turns a function for building a Continuation in a monadic computation
--   which is parameterized by the current state of the model
--   into a Continuation which reads the current state of the model,
--   runs the resulting monadic computation, and runs the Continuation
--   resulting from that computation.
kleisliT :: Applicative m => (model -> ContinuationT model m a) -> Continuation m model
kleisliT f = kleisli (pure . voidRunContinuationT . f)


instance Functor m => Functor (ContinuationT model m) where
  fmap f = ContinuationT . fmap (first f) . runContinuationT


instance Applicative m => Applicative (ContinuationT model m) where
  pure = ContinuationT . pure . (, done)

  ft <*> xt = ContinuationT $ do
    (\(f, fc) (x, xc) -> (f x, fc <> xc))
      <$> runContinuationT ft
      <*> runContinuationT xt


instance Monad m => Monad (ContinuationT model m) where
  return = ContinuationT . return . (, done)

  m >>= f = ContinuationT $ do
    (x, g) <- runContinuationT m
    (y, h) <- runContinuationT (f x)
    return (y, g <> h)


instance MonadTrans (ContinuationT model) where
  lift = ContinuationT . fmap (, done)


-- | Create an update to a constant value.
constUpdate :: a -> Continuation m a
constUpdate = pur . const
{-# INLINE constUpdate #-}
