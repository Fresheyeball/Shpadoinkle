{-# LANGUAGE RankNTypes #-}


module Shpadoinkle.Lens where


import           Control.Lens
import           Data.Maybe

import           Shpadoinkle


--embed :: Functor m => Lens s t a b -> (a -> Html m b) -> s -> Html m t
--embed = (%%~)


embed :: Functor m => s -> ASetter' s a -> Html m a -> Html m s
embed big len = fmap $ \s -> big & len .~ s


(<+) :: Functor m => s -> Lens' s a -> (a -> Html m a) -> Html m s
(<+) big len comp = (\s -> big & len .~ s) <$> comp (big ^. len)


infixl 8 <+


fracIntegral :: forall s a. Integral a => RealFrac s => Prism' s a
fracIntegral = prism' fromIntegral $
  \f -> let r = round f in
    if fromIntegral r == f then Just r else Nothing


rounding :: Integral s => RealFrac a => Iso' s a
rounding = iso fromIntegral round


defaulting :: a -> Iso' (Maybe a) a
defaulting x = iso (fromMaybe x) Just
