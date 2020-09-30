{-# LANGUAGE RankNTypes #-}


{-|
  Lens combinators for Shpadoinkle applications.
-}


module Shpadoinkle.Lens (
  -- * Continuous Composition
  generalize
  , onSum, onRecord
  , mapLens, (%>)
  , forLens, (<%)
  -- * Misc Outlaws
  , rounding, defaulting, fracIntegral
  ) where


import           Control.Lens
import           Data.Maybe
import           Shpadoinkle.Continuation


generalize, onRecord :: forall f m s a. Functor m => Continuous f => Lens' s a -> f m a -> f m s
{-|
  Compose multiple Shpadoinkle views onto a product type, most frequently a record.
  Let's say we have @Html@ which produces 'Int's. And we need to use it in a view
  with more components. The model for such a view might be @(Int, String)@. To use
  our child @Html@ inside the parent, we can assign produced @Int@s to the parent
  tuple by using the '_1' lens like so.

  @
  child :: Html Int

  parent :: Html (Int, String)
  parent = div_
    [ button [ onClick (0, \"Reset\") [ text "Reset!" ]
    , generalize _1 child
    ]
  @
-}
generalize len = liftC (set len) (view len)
{-|
  Alias for 'generalize' with a name idiomatic to the common case of composing onto a record.
-}
onRecord = generalize
{-# INLINE onRecord #-}
{-# INLINE generalize #-}


{-|
  Split multiple Shpadoinkle views over a sum type. This is commonly the case when
  using a sum to represent pages in a single page application, but it's useful for
  any sum. For example, consider that you have a view with a model of @Either Int String@,
  and a child @Html@ that produces 'Int's. You can compose this child onto the parent
  using '_Left' traversal like so.

  @
  child :: Html Int

  parent :: Html (Either Int String)
  parent = div_
    [ button [ onClick (Right \"Reset\") ] [ text "Reset!" ]
    , onSum _Left child
    ]
  @
-}
onSum ::  forall f m s a. Applicative m => Continuous f => Traversal' s a -> f m a -> f m s
onSum p = liftCMay (set p) (preview p)
{-# INLINE onSum #-}


infixl 8 <%
infixr 8 %>


forLens, (<%) :: forall f m s a. Functor m => Continuous f => s -> Lens' s a -> (a -> f m a) -> f m s
{-|
   A variant of 'generalize' for the case where you might need to map the smaller value.

   @
   parent :: Html (Int, String)
   parent model = div_
    [ forLens model _1 $ \(i :: Int) ->
        if i < 10 then text \"too low\" else
          button [ onClick (i + 1) ] [ text \"Increment\" ]
    ]
   @
-}
forLens big len f = generalize len . f $ view len big
-- | Infix for 'forLens'
(<%) = forLens
{-# INLINE forLens #-}
{-# INLINE (<%) #-}


mapLens, (%>) :: forall f m s a. Functor m => Continuous f => (a -> f m a) -> s -> Lens' s a -> f m s
{-|
  Like 'forLens' but with the lambda as the first argument.
-}
mapLens f big len = forLens big len f
-- | Infix for 'mapLens'
(%>) = mapLens
{-# INLINE mapLens #-}
{-# INLINE (%>) #-}


fracIntegral :: forall s a. Integral a => RealFrac s => Prism' s a
fracIntegral = prism fromIntegral $
  \f -> let r = round f in
    if fromIntegral r == f then Right r else Left f


rounding :: forall a s. Integral s => RealFrac a => Iso' s a
rounding = iso fromIntegral round
{-# INLINE rounding #-}


defaulting :: a -> Iso' (Maybe a) a
defaulting x = iso (fromMaybe x) Just
{-# INLINE defaulting #-}
