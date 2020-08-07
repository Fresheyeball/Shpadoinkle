module Shpadoinkle.Widgets.Hover ( HoverState (..), WithHoverState, withHoverState ) where


import Data.Functor.Identity
import Shpadoinkle
import Shpadoinkle.Html hiding ( s )
import Shpadoinkle.Widgets.Types.Hover


withHoverState :: IsHtml h p => IsProp p e => (WithHoverState a -> h (WithHoverState a)) -> WithHoverState a -> h (WithHoverState a)
withHoverState f s@(x,_) = runIdentity . props (Identity . (++ [onMouseenter (x,Hover), onMouseleave (x,NotHover)])) $ f s
