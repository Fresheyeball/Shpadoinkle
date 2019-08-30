module Shpadoinkle.Widgets.Present where


import           Data.Text

import           Shpadoinkle.Html


class Humanize a where humanize :: a -> Text

class Present a where toView :: MonadJSM m => a -> Html m a
