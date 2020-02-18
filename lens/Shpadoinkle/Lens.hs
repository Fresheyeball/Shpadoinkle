{-# LANGUAGE RankNTypes #-}


module Shpadoinkle.Lens where


import           Control.Lens

import           Shpadoinkle


embed :: Functor m => Lens s t a b -> (a -> Html m b) -> s -> Html m t
embed = (%%~)
