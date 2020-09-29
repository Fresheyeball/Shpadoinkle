{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}


{-|
  The memo function remembers the last arguement passed, and the last result.
  When a new value is passed, it compares it with the previous value, if they match,
  the previous result is returned from memory. This is useful when computing Html
  is expensive and benefits from memoization.
-}


module Shpadoinkle.Html.Memo (
  -- * Variadic Class
  Memo (..)
  -- * Uniadic
  , memo1,  memo2,  memo3,  memo4,  memo5,  memo6,  memo7,  memo8,  memo9
  -- * Custom Equality
  , memo1', memo2', memo3', memo4', memo5', memo6', memo7', memo8', memo9'
  ) where


import           Data.IORef
import           System.IO.Unsafe


{-|
   Variadic ditzy memoizer that only recalls at most one thing.

   prop> memo = id
-}
class Memo f where memo :: f -> f
instance                   Eq a                                                  => Memo (a -> b)                                         where memo = memo1
instance {-# OVERLAPS #-} (Eq a, Eq b)                                           => Memo (a -> b -> c)                                    where memo = memo2
instance {-# OVERLAPS #-} (Eq a, Eq b, Eq c)                                     => Memo (a -> b -> c -> d)                               where memo = memo3
instance {-# OVERLAPS #-} (Eq a, Eq b, Eq c, Eq d)                               => Memo (a -> b -> c -> d -> e)                          where memo = memo4
instance {-# OVERLAPS #-} (Eq a, Eq b, Eq c, Eq d, Eq e)                         => Memo (a -> b -> c -> d -> e -> f)                     where memo = memo5
instance {-# OVERLAPS #-} (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f)                   => Memo (a -> b -> c -> d -> e -> f -> g)                where memo = memo6
instance {-# OVERLAPS #-} (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g)             => Memo (a -> b -> c -> d -> e -> f -> g -> h)           where memo = memo7
instance {-# OVERLAPS #-} (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h)       => Memo (a -> b -> c -> d -> e -> f -> g -> h -> i)      where memo = memo8
instance {-# OVERLAPS #-} (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => Memo (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) where memo = memo9

memo1' e f a = unsafePerformIO $ do
  r <- newIORef (a, f a)
  return $ applyEq e f r a
memo2' e f a b               = memo1' e (uncurry  f) (a,b)
memo3' e f a b c             = memo1' e (uncurry2 f) (a,b,c)
memo4' e f a b c d           = memo1' e (uncurry3 f) (a,b,c,d)
memo5' e f a b c d g         = memo1' e (uncurry4 f) (a,b,c,d,g)
memo6' e f a b c d g h       = memo1' e (uncurry5 f) (a,b,c,d,g,h)
memo7' e f a b c d g h i     = memo1' e (uncurry6 f) (a,b,c,d,g,h,i)
memo8' e f a b c d g h i j   = memo1' e (uncurry7 f) (a,b,c,d,g,h,i,j)
memo9' e f a b c d g h i j k = memo1' e (uncurry8 f) (a,b,c,d,g,h,i,j,k)

memo1 :: Eq a =>                                                                 (a -> b)                                         -> a -> b
memo2 :: Eq a => Eq b =>                                                         (a -> b -> c)                                    -> a -> b -> c
memo3 :: Eq a => Eq b => Eq c =>                                                 (a -> b -> c -> d)                               -> a -> b -> c -> d
memo4 :: Eq a => Eq b => Eq c => Eq d =>                                         (a -> b -> c -> d -> e)                          -> a -> b -> c -> d -> e
memo5 :: Eq a => Eq b => Eq c => Eq d => Eq e =>                                 (a -> b -> c -> d -> e -> f)                     -> a -> b -> c -> d -> e -> f
memo6 :: Eq a => Eq b => Eq c => Eq d => Eq e => Eq f =>                         (a -> b -> c -> d -> e -> f -> g)                -> a -> b -> c -> d -> e -> f -> g
memo7 :: Eq a => Eq b => Eq c => Eq d => Eq e => Eq f => Eq g =>                 (a -> b -> c -> d -> e -> f -> g -> h)           -> a -> b -> c -> d -> e -> f -> g -> h
memo8 :: Eq a => Eq b => Eq c => Eq d => Eq e => Eq f => Eq g => Eq h =>         (a -> b -> c -> d -> e -> f -> g -> h -> i)      -> a -> b -> c -> d -> e -> f -> g -> h -> i
memo9 :: Eq a => Eq b => Eq c => Eq d => Eq e => Eq f => Eq g => Eq h => Eq i => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j

memo1 = memo1' (/=)
memo2 = memo2' (/=)
memo3 = memo3' (/=)
memo4 = memo4' (/=)
memo5 = memo5' (/=)
memo6 = memo6' (/=)
memo7 = memo7' (/=)
memo8 = memo8' (/=)
memo9 = memo9' (/=)

uncurry2 f (a,b,c)             = f a b c
uncurry3 f (a,b,c,d)           = f a b c d
uncurry4 f (a,b,c,d,e)         = f a b c d e
uncurry5 f (a,b,c,d,e,g)       = f a b c d e g
uncurry6 f (a,b,c,d,e,g,h)     = f a b c d e g h
uncurry7 f (a,b,c,d,e,g,h,i)   = f a b c d e g h i
uncurry8 f (a,b,c,d,e,g,h,i,j) = f a b c d e g h i j

applyEq :: (a -> a -> Bool) -> (a -> b) -> IORef (a, b) -> a -> b
applyEq e f r a = unsafePerformIO $ do
  (a', b) <- readIORef r
  if not $ e a' a then return b else do
    let b' = f a
    writeIORef r (a', b')
    return b'

