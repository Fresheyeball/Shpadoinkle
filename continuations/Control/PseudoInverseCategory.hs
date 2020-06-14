{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}


module Control.PseudoInverseCategory
  ( ToHask (..)
  , HasHaskFunctors (..)
  , PseudoInverseCategory (..)
  , PIArrow (..)
  , piswap
  , piassoc
  , EndoIso (..)
  , pimap
  ) where


import           Prelude hiding ((.), id)
import qualified Control.Categorical.Functor as F
import           Control.Category
import           Data.Functor.Identity
import           Data.Tuple (swap)


-- | A functor from another category to Hask. Laws:
-- ```Haskell
--   piapply (f . g) = piapply f . piapply g
--   piapply id = id
-- ```
class Category a => ToHask a where
  piapply :: a x y -> x -> y


class Category a => HasHaskFunctors a where
  fmapA :: Functor f => a x y -> a (f x) (f y)


-- | A pseudo-inverse category is a category where every morphism has a pseudo-inverse.
--  What this means is defined by the following laws (and perhaps things can be removed
--  and perhaps things should be added):
-- ```Haskell
--   pipower 1 f = f
--   pileft (pipower 0 f) = id
--   piright (pipower 0 f) = id
--   pipower (n+1) f = pileft f . pipower n f
--   piinverse (piinverse f) = f
--   f . piinverse f = piright (pipower 2 f)
--   piinverse f . f = pileft (pipower 2 f)
--   pileft (piright f) = piright (piright f) = piright f
--   piright (pileft f) = pileft (pileft f) = pileft f
--   piinverse (pileft f) = pileft f
--   piinverse (piright f) = piright f
-- ```
class Category a => PseudoInverseCategory a where
  -- | Apply a morphism n times, n >= 0.
  pipower :: Int -> a x y -> a x y

  -- | Change a morphism into an endomorphism of its domain.
  pileft :: a x y -> a x x

  -- | Change a morphism into an endomorphism of its codomain.
  piright :: a x y -> a y y

  -- | Pseudo-invert a morphism. The pseudo-inverse of a morphism may or may not
  --   be its inverse. f is the inverse of g means that f.g = id = g.f.
  --   If f has an inverse, then minverse f may or may not be the inverse
  --   of f.
  piinverse :: a x y -> a y x


-- | An analogue of the Arrow typeclass for pseudo-inverse categories. Laws:
-- ```Haskell
--   piiso id id = id
--   piendo id = id
--   piiso (f . g) (h . i) = piiso f h . piiso g i
--   piendo (f . h) = piendo f . piendo h
--   pifirst (piiso f g) = piiso (first f) (first g)
--   pifirst (piendo f) = piendo (first f)
--   pifirst (f . g) = pifirst f . pifirst g
--   pisplit id g . pifirst f = pifirst f . pisplit id g
--   piassoc . first (first f) = first f . piassoc
--   pisecond f = piswap . pifirst f . piswap
--   pisplit f g = pifirst f . pisecond g
--   pifan f g = piiso (\b -> (b,b)) fst . pisplit f g
--   piinverse (piiso f g) = piiso g f
--   piinverse (piendo f) = piendo f
--   piapply (piiso f g) = f
--   piapply (piinverse (piiso f g)) = g
--   piapply (piendo f) = f
--   
-- ```
class PseudoInverseCategory a => PIArrow a where
  -- | Create an arrow from an isomorphism (restricted version of arr).
  piiso :: (b -> c) -> (c -> b) -> a b c

  -- | Create an arrow from an endomorphism (restricted version of arr).
  piendo :: (b -> b) -> a b b

  -- | Apply an arrow to the first coordinate of a tuple.
  pifirst :: a b c -> a (b, d) (c, d)

  -- | Apply an arrow to the second coordinate of a tuple.
  pisecond :: a b c -> a (d, b) (d, c)

  -- | Combine two arrows to work in parallel on a tuple.
  pisplit :: a b c -> a d e -> a (b, d) (c, e)

  -- | Combine two arrows on the same input to output a tuple.
  pifan :: a b c -> a b d -> a b (c, d)


-- | Every pseudo-inverse category has isomorphisms to swap the coordinates of a tuple.
piswap :: PIArrow a => a (b, c) (c, b)
piswap = piiso swap swap


-- | Every pseudo-inverse category has isomorphisms to change the associativity of a 3-tuple.
piassoc :: PIArrow a => a ((b,c),d) (b,(c,d))
piassoc = piiso (\((x,y),z) -> (x,(y,z))) (\(x,(y,z)) -> ((x,y),z))


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


instance F.Functor EndoIso (->) Identity where
  map (EndoIso f g _) = Identity . g . f . runIdentity


instance ToHask EndoIso where
  piapply (EndoIso f g _) = g.f


pimap :: F.Functor EndoIso EndoIso f => EndoIso a b -> f a -> f b
pimap = (\(EndoIso f g _) -> g.f) . F.map


instance HasHaskFunctors EndoIso where
  fmapA (EndoIso f g h) = EndoIso (fmap f) (fmap g) (fmap h)


instance PseudoInverseCategory EndoIso where
  pipower n (EndoIso f g h)
    | n < 0 = error "pipower with n < 0"
    | n > 0 = let EndoIso f' _ _ = pipower (n-1) (EndoIso f g h) in EndoIso (f.f') g h
    | otherwise = EndoIso id g h
  pileft (EndoIso f _ _) = EndoIso f id id
  piright (EndoIso f g h) = EndoIso (g.f.h) id id
  piinverse (EndoIso f g h) = EndoIso (g.f.h) h g


instance PIArrow EndoIso where
  piiso = EndoIso id
  piendo f = EndoIso f id id
  pifirst (EndoIso f g h) = EndoIso (\(x,y)->(f x,y)) (\(x,y)->(g x,y)) (\(x,y)->(h x,y))
  pisecond (EndoIso f g h) = EndoIso (\(x,y)->(x,f y)) (\(x,y)->(x,g y)) (\(x,y)->(x,h y))
  pisplit (EndoIso f g h) (EndoIso i j k) = EndoIso
    (\(x,y) -> (f x, i y))
    (\(x,y) -> (g x, j y))
    (\(x,y) -> (h x, k y))
  pifan (EndoIso f g h) (EndoIso i j _) = EndoIso
    (\x -> f (i x))
    (\x -> (g x, j x))
    (\(x,_) -> h x) -- it shouldn't matter which side we use to go back because we have isomorphisms

