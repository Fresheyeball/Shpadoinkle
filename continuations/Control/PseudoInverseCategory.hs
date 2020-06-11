{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.PseudoInverseCategory
  ( ToHask (..)
  , HasHaskFunctors (..)
  , PseudoInverseCategory (..)
  , PIArrow (..)
  , mswap
  , massoc
  , EndoIso (..)
  ) where

import Prelude hiding ((.), id)
import Control.Category
import Data.Tuple (swap)

-- | A functor from another category to Hask. Laws:
-- ```Haskell
--   mapply (f . g) = mapply f . mapply g
--   mapply id = id
-- ```
class Category a => ToHask a where
  mapply :: a x y -> x -> y

class Category a => HasHaskFunctors a where
  fmapA :: Functor f => a x y -> a (f x) (f y)

-- | A pseudo-inverse category is a category where every morphism has a pseudo-inverse.
--  What this means is defined by the following laws (and perhaps things can be removed
--  and perhaps things should be added):
-- ```Haskell
--   mpower 1 f = f
--   mleft (mpower 0 f) = id
--   mright (mpower 0 f) = id
--   mpower (n+1) f = mleft f . mpower n f
--   minverse (minverse f) = f
--   f . minverse f = mright (mpower 2 f)
--   minverse f . f = mleft (mpower 2 f)
--   mleft (mright f) = mright (mright f) = mright f
--   mright (mleft f) = mleft (mleft f) = mleft f
--   minverse (mleft f) = mleft f
--   minverse (mright f) = mright f
-- ```
class Category a => PseudoInverseCategory a where
  mpower :: Int -> a x y -> a x y -- requires n >= 0
  mleft :: a x y -> a x x
  mright :: a x y -> a y y
  minverse :: a x y -> a y x

-- | An analogue of the Arrow typeclass for pseudo-inverse categories. Laws:
-- ```Haskell
--   miso id id = id
--   mendo id = id
--   miso (f . g) (h . i) = miso f h . miso g i
--   mendo (f . h) = mendo f . mendo h
--   mfirst (miso f g) = miso (first f) (first g)
--   mfirst (mendo f) = mendo (first f)
--   mfirst (f . g) = mfirst f . mfirst g
--   msplit id g . mfirst f = mfirst f . msplit id g
--   massoc . first (first f) = first f . massoc
--   msecond f = mswap . mfirst f . mswap
--   msplit f g = mfirst f . msecond g
--   mfan f g = miso (\b -> (b,b)) fst . msplit f g
--   minverse (miso f g) = miso g f
--   minverse (mendo f) = mendo f
--   mapply (miso f g) = f
--   mapply (minverse (miso f g)) = g
--   mapply (mendo f) = f
-- ```
class PseudoInverseCategory a => PIArrow a where
  miso :: (b -> c) -> (c -> b) -> a b c
  mendo :: (b -> b) -> a b b
  mfirst :: a b c -> a (b, d) (c, d)
  msecond :: a b c -> a (d, b) (d, c)
  msplit :: a b c -> a d e -> a (b, d) (c, e)
  mfan :: a b c -> a b d -> a b (c, d)

mswap :: PIArrow a => a (b, c) (c, b)
mswap = miso swap swap

massoc :: PIArrow a => a ((b,c),d) (b,(c,d))
massoc = miso (\((x,y),z) -> (x,(y,z))) (\(x,(y,z)) -> ((x,y),z))

-- | This is a pseudo-inverse category where a morphism is a composition of an endomorphism
--   on the domain and an isomorphism of the domain with the codomain.
--   The last two arguments are required to form an isomorphism, i.e. for all EndoIso f g h:
-- ```Haskell
--   g.h = id
--   h.g = id
-- ```
--
-- This category contains as objects all types in Hask and as morphisms all compositions
-- of endomorphisms and isomorphisms in Hask.
data EndoIso a b = EndoIso (a -> a) (a -> b) (b -> a)

instance Category EndoIso where
  id = EndoIso id id id

  EndoIso i j k . EndoIso f g h = EndoIso (f . h . i . g) (j . g) (h . k)

instance ToHask EndoIso where
  mapply (EndoIso f g _) = g.f

instance HasHaskFunctors EndoIso where
  fmapA (EndoIso f g h) = EndoIso (fmap f) (fmap g) (fmap h)

instance PseudoInverseCategory EndoIso where
  mpower n (EndoIso f g h)
    | n < 0 = error "mpower with n < 0"
    | n > 0 = let EndoIso f' _ _ = mpower (n-1) (EndoIso f g h) in EndoIso (f.f') g h
    | otherwise = EndoIso id g h
  mleft (EndoIso f _ _) = EndoIso f id id
  mright (EndoIso f g h) = EndoIso (g.f.h) id id
  minverse (EndoIso f g h) = EndoIso (g.f.h) h g

instance PIArrow EndoIso where
  miso = EndoIso id
  mendo f = EndoIso f id id
  mfirst (EndoIso f g h) = EndoIso (\(x,y)->(f x,y)) (\(x,y)->(g x,y)) (\(x,y)->(h x,y))
  msecond (EndoIso f g h) = EndoIso (\(x,y)->(x,f y)) (\(x,y)->(x,g y)) (\(x,y)->(x,h y))
  msplit (EndoIso f g h) (EndoIso i j k) = EndoIso
    (\(x,y) -> (f x, i y))
    (\(x,y) -> (g x, j y))
    (\(x,y) -> (h x, k y))
  mfan (EndoIso f g h) (EndoIso i j _) = EndoIso
    (\x -> f (i x))
    (\x -> (g x, j x))
    (\(x,_) -> h x) -- it shouldn't matter which side we use to go back because we have isomorphisms

