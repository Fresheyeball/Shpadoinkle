module Shpadoinkle.Html.Event.HandlerTransformer ( HandlerTransformer ) where


import Data.Text
import Shpadoinkle


type HandlerTransformer m a b = (a -> (Text, PropM m b)) -> (a -> (Text, PropM m b))
