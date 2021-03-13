module Shpadoinkle.Template where


import           Data.Text
import           Shpadoinkle


template :: (Text -> Text) -> Html m a -> Html m a
template r (Html html) = Html $ \n p t ->
  html (\tn ps cs ->
    n (r tn)
      ((\(k,v) -> (r k, case v of PText pt -> PText (r pt); x -> x)) <$> ps)
      cs)
    p $
    \t' -> t $ r t'


