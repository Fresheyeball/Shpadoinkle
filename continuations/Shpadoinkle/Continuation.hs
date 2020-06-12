{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Shpadoinkle.Continuation
  ( Continuation (..)
  , pur
  , impur
  , runContinuation
  , MapContinuations (..)
  , convertC
  , liftC
  , liftMC
  , leftC
  , leftMC
  , rightC
  , rightMC
  , maybeC
  , maybeMC
  , writeUpdate
  , shouldUpdate
  ) where

import qualified Control.Categorical.Functor as F
import           Control.Monad (liftM2)
import           Control.PseudoInverseCategory
import           GHC.Conc (retry)
import           UnliftIO
import           UnliftIO.Concurrent

-- | The type of a state update in Shpadoinkle. A Continuation builds up an
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
--   (even if it changed since the start of computing the continuation), and the updates made
--   so far, although those updates are not committed to the real state until the continuation
--   finishes and they are all done atomically together.
--
--   You can combine multiple continuations over the type `a` using the Monoid typeclass
--   instance. The resulting continuation will execute all the subcontinuations in parallel,
--   allowing them to see each other's state updates and roll back each other's updates,
--   applying all of the updates generated by all the subcontinuations atomically once
--   all of them are done.
data Continuation m a = Continuation (a -> a, a -> m (Continuation m a))
                      | Rollback (Continuation m a)
                      | Done

-- | A pure state updating function can be turned into a Continuation.
pur :: Applicative m => (a -> a) -> Continuation m a
pur = Continuation . (, const (pure Done))

-- | A monadic IO computation of a pure state updating function can be turned into a Continuation.
impur :: Monad m => m (a -> a) -> Continuation m a
impur m = Continuation . (id,) . const $ do
  f <- m
  return $ Continuation (f, const (return Done))

-- | runContinuation takes a Continuation and a state value and runs the whole continuation
--   as if the real state was frozen at the value given to runContinuation. It performs all the
--   IO actions in the stages of the continuation and returns a pure state updating function
--   which is the composition of all the pure state updating functions generated by the
--   non-rolled-back stages of the continuation. If you are trying to update a Continuous
--   territory, then you should probably be using writeUpdate instead of runContinuation,
--   because writeUpdate will allow each stage of the continuation to see any extant updates
--   made to the territory after the continuation started running.
runContinuation :: Monad m => Continuation m a -> a -> m (a -> a)
runContinuation = runContinuation' id

runContinuation' :: Monad m => (a -> a) -> Continuation m a -> a -> m (a -> a)
runContinuation' f (Continuation (g, h)) x = do
  i <- h (f x)
  runContinuation' (g.f) i x
runContinuation' _ (Rollback f) x = runContinuation' id f x
runContinuation' f Done _ = return f

class MapContinuations f where
  mapMC :: Functor m => (Continuation m a -> Continuation m b) -> f m a -> f m b

instance MapContinuations Continuation where
  mapMC = id

convertC :: Functor m => (forall b. m b -> n b) -> Continuation m a -> Continuation n a
convertC _ Done = Done
convertC f (Rollback r) = Rollback (convertC f r)
convertC f (Continuation (g, h)) = Continuation . (g,) $ \x -> f $ convertC f <$> h x

liftC :: Functor m => (b -> a -> b) -> (b -> a) -> Continuation m a -> Continuation m b
liftC _ _ Done = Done
liftC f g (Rollback r) = Rollback (liftC f g r)
liftC f g (Continuation (h, i)) = Continuation (\x -> f x (h (g x)), \x -> liftC f g <$> i (g x))

liftMC :: Functor m => MapContinuations f => (b -> a -> b) -> (b -> a) -> f m a -> f m b
liftMC f g = mapMC (liftC f g)

leftC :: Functor m => Continuation m a -> Continuation m (a,b)
leftC = liftC (\(_,y) x -> (x,y)) fst

leftMC :: Functor m => MapContinuations f => f m a -> f m (a,b)
leftMC = mapMC leftC

rightC :: Functor m => Continuation m b -> Continuation m (a,b)
rightC = liftC (\(x,_) y -> (x,y)) snd

rightMC :: Functor m => MapContinuations f => f m b -> f m (a,b)
rightMC = mapMC rightC

maybeC :: Applicative m => Continuation m a -> Continuation m (Maybe a)
maybeC Done = Done
maybeC (Rollback r) = Rollback (maybeC r)
maybeC (Continuation (f, g)) = Continuation . (fmap f,) $
  \case
    Just x -> maybeC <$> g x
    Nothing -> pure Done

maybeMC :: Applicative m => MapContinuations f => f m a -> f m (Maybe a)
maybeMC = mapMC maybeC

contIso :: Functor m => (a -> b) -> (b -> a) -> Continuation m a -> Continuation m b
contIso f g (Continuation (h, i)) = Continuation (f.h.g, fmap (contIso f g) . i . g)
contIso f g (Rollback h) = Rollback (contIso f g h)
contIso _ _ Done = Done

instance Applicative m => F.Functor EndoIso EndoIso (Continuation m) where
  map (EndoIso f g h) =
    EndoIso (Continuation . (f,) . const . pure) (contIso g h) (contIso h g)

instance Monad m => Semigroup (Continuation m a) where
  (Continuation (f, g)) <> (Continuation (h, i)) =
    Continuation (f.h, \x -> liftM2 (<>) (g x) (i x))
  (Continuation (f, g)) <> (Rollback h) =
    Rollback (Continuation (f, (\x -> liftM2 (<>) (g x) (return h))))
  (Rollback h) <> (Continuation (_, g)) =
    Rollback (Continuation (id, \x -> liftM2 (<>) (return h) (g x)))
  (Rollback f) <> (Rollback g) = Rollback (f <> g)
  f <> Done = f
  Done <> f = f

instance Monad m => Monoid (Continuation m a) where
  mempty = Done

writeUpdate' :: MonadUnliftIO m => (a -> a) -> TVar a -> (a -> m (Continuation m a)) -> m ()
writeUpdate' h model f = do
  i <- readTVarIO model
  m <- f (h i)
  case m of
    Continuation (g,gs) -> writeUpdate' (g.h) model gs
    Done -> atomically $ writeTVar model =<< h <$> readTVar model
    Rollback gs -> writeUpdate' id model (const (return gs))

writeUpdate :: MonadUnliftIO m => TVar a -> (a -> m (Continuation m a)) -> m ()
writeUpdate = writeUpdate' id

shouldUpdate :: MonadUnliftIO m => Eq a => (b -> a -> m b) -> b -> TVar a -> m ()
shouldUpdate sun prev model = do
  i' <- readTVarIO model
  p  <- newTVarIO i'
  () <$ forkIO (go prev p)
  where
    go x p = do
      a <- atomically $ do
        new' <- readTVar model
        old  <- readTVar p
        if new' == old then retry else new' <$ writeTVar p new'
      y <- sun x a
      go y p
