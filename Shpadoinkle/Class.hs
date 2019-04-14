{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}


module Shpadoinkle.Core
  ( Html (..), Prop (..)
  , mapHtml,   mapProp
  , Shpadoinkle (..)
  , shpadoinkle
  , RawNode (..)
  ) where


import           Control.Isomorphic
import           Control.Natural
import           GHC.Conc                    (retry)
import           Language.Javascript.JSaddle hiding (( # ))


data Html :: (Type -> Type) -> Type -> Type where
  Node :: Text -> [(Text, Prop m o)] -> [Html m o] -> Html m o
  TextNode :: Text -> Html m o


mapHtml :: (m :~> n) -> Html m o -> Html n o
mapHtml f = \case
  Node t props children -> Node t (fmap (mapProp f) <$> props) (mapHtml f <$> children)
  TextNode t -> TextNode t


mapProp :: (m :~> n) -> Prop m o -> Prop n o
mapProp f = \case
  PListener g -> PListener (f # g)
  PText t     -> PText t
  PFlag       -> PFlag


deriving instance Functor m => Functor (Html m)


data Prop m o = PText Text | PListener (m o) | PFlag


deriving instance Functor m => Functor (Prop m)


instance IsString (Html m o) where
  fromString = TextNode . to


instance IsString (Prop m o) where
  fromString = PText . to


instance {-# OVERLAPPING #-} IsString [(Text, Prop m o)] where
  fromString = pure . ("className", ) . PText . to


newtype RawNode  = RawNode { unRawNode :: JSVal }
instance ToJSVal   RawNode where toJSVal   = return . unRawNode
instance FromJSVal RawNode where fromJSVal = return . Just . RawNode


class Shpadoinkle t m a | t m -> a where
  type VNode t m
  interpret :: (m :~> JSM) -> Html (t m) a -> t m (VNode t m)
  patch :: RawNode -> Maybe (VNode t m) -> VNode t m -> t m (VNode t m)
  setup :: JSM () -> t m ()


shpadoinkle
  :: Shpadoinkle t m a
  => Eq a
  => MonadIO (t m)
  => (m :~> JSM)
  -> (t m :~> m)
  -> TVar a
  -> (a -> Html (t m) a)
  -> t m RawNode
  -> JSM ()
shpadoinkle toJSM toM model view stage = do
  let
    j = toJSM . toM

    go c n p = do
      a  <- unwrapNT j . atomically $ do
        new' <- readTVar model
        old  <- readTVar p
        if new' == old then retry else new' <$ writeTVar p new'
      m  <- j # interpret toJSM (view a)
      n' <- j # patch c (Just n) m
      go c n' p

  i' <- readTVarIO model
  p  <- newTVarIO i'
  unwrapNT j . setup $ do
    c <- j # stage
    n <- j # interpret toJSM (view i')
    async $ go c n p
    j # patch c Nothing n
    return ()
