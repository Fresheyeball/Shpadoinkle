{-# LANGUAGE ExplicitForAll #-}


module Shpadoinkle.Html.Memo where


import           Data.IORef
import           System.IO.Unsafe


memo' :: (a -> a -> Bool) -> (a -> b) -> a -> b
memo' e f a = unsafePerformIO $ do
  r <- newIORef (a, f a)
  return $ applyEq e f r a


memo :: Eq a => (a -> b) -> a -> b
memo = memo' (/=)


memo2' :: ((a,b) -> (a,b) -> Bool) -> (a -> b -> c) -> a -> b -> c
memo2' e f a b = memo' e (uncurry f) (a, b)


memo2 :: Eq a => Eq b => (a -> b -> c) -> a -> b -> c
memo2 = memo2' (/=)


memo3' :: ((a,b,c) -> (a,b,c) -> Bool) -> (a -> b -> c -> d) -> a -> b -> c -> d
memo3' e f a b c = memo' e (uncurry2 f) (a, b, c)


memo3 :: Eq a => Eq b => Eq c => (a -> b -> c -> d) -> a -> b -> c -> d
memo3 = memo3' (/=)


uncurry2 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry2 f (a,b,c) = f a b c


memo4' :: ((a,b,c,d) -> (a,b,c,d) -> Bool) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e
memo4' e f a b c d = memo' e (uncurry3 f) (a,b,c,d)


memo4 :: Eq a => Eq b => Eq c => Eq d => (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e
memo4 = memo4' (/=)


uncurry3 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry3 f (a,b,c,d) = f a b c d


applyEq :: (a -> a -> Bool) -> (a -> b) -> IORef (a, b) -> a -> b
applyEq e f r a = unsafePerformIO $ do
  (a', b) <- readIORef r
  if not $ e a' a then return b else do
    let b' = f a
    writeIORef r (a', b')
    return b'

