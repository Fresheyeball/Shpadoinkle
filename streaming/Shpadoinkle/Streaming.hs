{-# LANGUAGE ScopedTypeVariables #-}


module Shpadoinkle.Streaming
  ( consumeStream
  ) where


import Shpadoinkle hiding (h)

import Data.Functor.Of (Of ((:>)))
import Streaming (Stream)
import Streaming.Internal (destroy)


consumeStream :: forall m a b. Monad m => Stream (Of a) m () -> (a -> m (b -> b)) -> Continuation m b
consumeStream stream f = destroy stream g h j
  where
    g :: Of a (Continuation m b) -> Continuation m b
    g (a :> k) = voidRunContinuationT $ do
      commit (impur (f a))
      commit (merge k)

    h :: m (Continuation m b) -> Continuation m b
    h = causedBy

    j :: () -> Continuation m b
    j = const done
