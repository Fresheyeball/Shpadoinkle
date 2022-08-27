module Shpadoinkle.Website.Component.Hoogle where


import           Shpadoinkle.Html
import           Shpadoinkle.Website.Types


hoogleWidget :: forall m. Hooglable m => MonadJSM m => Hoogle -> Html m Hoogle
hoogleWidget hoo =
  div
  [ onInputM (query . Search) ]
  [ onRecord #search $ I.search [] (search hoo)
  , onRecord #targets $ div [ class' T.p_2 ] [ dropdown theme defConfig $ targets hoo ]
  ]

 where

 query :: Search -> m (Hoogle -> Hoogle)
 query ss = do
   ts <- findTargets ss
   return $ #targets <>~ Nothing `withOptions` ts


targetWidget :: Target -> Html m a
targetWidget = div' . pure . innerHTML . pack . targetItem
