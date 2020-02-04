{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Shpadoinkle.Widgets.Form.Dropdown where


import           Control.Compactable
import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Prelude                   hiding (div)


import           Shpadoinkle
import           Shpadoinkle.Html          hiding (p, s, s', selected)
import           Shpadoinkle.Keyboard
import           Shpadoinkle.Widgets.Types


default (Text)


data Dropdown p a = Dropdown
  { _considered :: ConsideredChoice p a
  , _toggle     :: Toggle
  }


deriving instance (Show (Selected p a), Show (Considered p a), Show a)        => Show (Dropdown p a)
deriving instance (Read (Selected p a), Read (Considered p a), Read a, Ord a) => Read (Dropdown p a)
deriving instance (Eq   (Selected p a), Eq   (Considered p a), Eq a)          => Eq   (Dropdown p a)
deriving instance (Ord  (Selected p a), Ord  (Considered p a), Ord a)         => Ord  (Dropdown p a)
deriving instance Generic (Dropdown p a)
instance (ToJSON a,   ToJSON (Selected p a),   ToJSON (Considered p a))          => ToJSON   (Dropdown p a)
instance (FromJSON a, FromJSON (Selected p a), FromJSON (Considered p a), Ord a) => FromJSON (Dropdown p a)


instance (Consideration ConsideredChoice p, Ord a)
    => IsToggle (Dropdown p a) where
  close  p = shrug $ p { _toggle = close  (_toggle p) }
  toggle p = shrug $ p { _toggle = toggle (_toggle p) }
  open   p = shrug $ p { _toggle = open   (_toggle p) }


newtype Config m = Config
  { _attrs :: forall a. [(Text, Prop m a)] }


defConfig :: Config m
defConfig = Config []


instance (Compactable (ConsideredChoice p)) => Compactable (Dropdown p) where
  compact (Dropdown c t) = Dropdown (compact c) t
  separate (Dropdown c t) = let (l,r) = separate c in (Dropdown l t, Dropdown r t)
  filter p (Dropdown c t) = Dropdown (Control.Compactable.filter p c) t
  partition p (Dropdown c t) = let (l, r) = Control.Compactable.partition p c in (Dropdown l t, Dropdown r t)


instance Semigroup (ConsideredChoice p a) => Semigroup (Dropdown p a) where
  Dropdown c t <> Dropdown c' t' = Dropdown (c <> c') (t <> t')


instance Monoid (ConsideredChoice p a) => Monoid (Dropdown p a) where
  mempty = Dropdown mempty mempty


instance SetLike (ConsideredChoice p) => SetLike (Dropdown p) where
  toSet = toSet . _considered
  smap f (Dropdown c t) = Dropdown (smap f c) t
  valid (Dropdown c _) = valid c


instance Consideration ConsideredChoice p => Selection Dropdown p where
  select  (Dropdown c t) x = close $ Dropdown (select c x) t
  select' (Dropdown c t) x = close $ Dropdown (select' c x) t
  unselected = unselected . _considered
  selected   = selected . _considered
  withOptions x xs  = Dropdown (x `withOptions` xs) mempty
  withOptions' x xs = Dropdown (x `withOptions'` xs) mempty


instance (Consideration ConsideredChoice p, Deselection ConsideredChoice p)
    => Deselection Dropdown p where
  noselection xs = Dropdown (noselection xs) mempty
  deselect (Dropdown c t) = close $ Dropdown (deselect c) t


instance Consideration ConsideredChoice p => Consideration Dropdown p where
  consider  x (Dropdown c t) = Dropdown (consider x c) t
  consider' x (Dropdown c t) = Dropdown (consider' x c) t
  choose (Dropdown c t) = Dropdown (choose c) t
  choice (Dropdown c _) = choice c
  considered (Dropdown c _) = considered c
  shrug (Dropdown c xs) = Dropdown (shrug c) xs


data Theme m = Theme
  { _wrapper :: forall a. [Html m a] -> Html m a
  , _header  :: forall a. [Html m a] -> [Html m a]
  , _list    :: forall a. [Html m a] -> Html m a
  , _item    :: forall a. [Html m a] -> Html m a
  }


bootstrap :: Dropdown p b -> Theme m
bootstrap Dropdown {..} = Theme
  { _wrapper = div
    [ className [ ("dropdown", True)
                , ("show", _toggle == Open) ]
    ]
  , _header  = pure . button
    [ className [ "btn", "btn-secondary", "dropdown-toggle" ]
    , type' "button"
    ]
  , _list    = div
    [ className [ ("dropdown-menu", True)
                , ("show", _toggle == Open) ]
    ]
  , _item    = a [ className "dropdown-item", href "#" ]
  }


semantic :: Dropdown p b -> Theme m
semantic Dropdown {..} = Theme
  { _wrapper = div
    [ className [ ("dropdown", True)
                , ("ui",       True)
                , ("active", _toggle == Open) ]
    ]
  , _header  = \cs ->
    [ div [ class' "text" ] cs
    , i' [ className ["dropdown", "icon"] ]
    ]
  , _list    = div
    [ className [ "menu"
                , "transition" ]
    ]
  , _item    = div [ className "item" ]
  }


act :: ( Considered p ~ Maybe
       , Consideration ConsideredChoice p
       , Consideration Dropdown p
       , Ord a)
    => Dropdown p a -> Dropdown p a
act x | _toggle x == Open =
  close $ case considered x of
    Just _ -> choose x
    _      -> x
act x | otherwise = open x


dropdown ::
  ( Considered p ~ Maybe
  , Consideration Dropdown p
  , Consideration ConsideredChoice p
  , Present (Selected p a), Present a, Ord a
  , MonadJSM m
  ) => (forall b. Dropdown p b -> Theme m)
    -> Config m -> Dropdown p a -> Html m (Dropdown p a)
dropdown toTheme Config {..} x =
  let
    Theme {..} = toTheme x
  in injectProps
  ([onKeyup' $ \case
    Enter     -> act x
    UpArrow   -> considerPrev x
    DownArrow -> considerNext x
    _ -> x
  , onClick $ act x
  , tabbable
  ] ++ _attrs) . _wrapper $
  (_header . present $ selected x) ++
  [ _list $ (\y -> injectProps
    [ onMouseover $ consider' y x
    , onFocus     $ consider' y x
    , onClick     $ select' x y
    , tabbable
    ] . _item $ present y) <$> toList (unselected x)
  ]
