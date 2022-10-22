{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
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
#ifdef TESTING
import           Test.QuickCheck           (Arbitrary (..))
#endif


import           Shpadoinkle
import           Shpadoinkle.Html          hiding (p, s, s', select, select',
                                            selected)
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
deriving instance (Foldable (ConsideredChoice p)) => Foldable (Dropdown p)
deriving instance Generic (Dropdown p a)
instance (ToJSON a,   ToJSON (Selected p a),   ToJSON (Considered p a))          => ToJSON   (Dropdown p a)
instance (FromJSON a, FromJSON (Selected p a), FromJSON (Considered p a), Ord a) => FromJSON (Dropdown p a)
instance (NFData (Selected p a), NFData (ConsideredChoice p a), NFData a) => NFData (Dropdown p a)


instance Control (Dropdown 'One) where
  type Val (Dropdown 'One) a = Maybe a
  hygiene  :: Applicative f          => (Hygiene -> f Hygiene)   -> Dropdown 'One a -> f (Dropdown 'One a)
  hygiene f d = (\x -> d {_toggle = Closed x }) <$> f (togHygiene $ _toggle d)
  value    :: (Applicative f, Ord a) => (Maybe a -> f (Maybe a)) -> Dropdown 'One a -> f (Dropdown 'One a)
  value   f d = maybe d (select' d) <$> f (selected d)

instance Control (Dropdown 'AtleastOne) where
  hygiene f d = (\x -> d {_toggle = Closed x }) <$> f (togHygiene $ _toggle d)
  value f d = select' d <$> f (selected d)


instance (Consideration ConsideredChoice p, Ord a)
    => IsToggle (Dropdown p a) where
  close  p = shrug $ p { _toggle = close  (_toggle p) }
  toggle p = shrug $ p { _toggle = toggle (_toggle p) }
  open   p = shrug $ p { _toggle = open   (_toggle p) }


data Config m = Config
  { _attrs     :: forall a. [(Text, Prop m a)]
  , _clickAway :: ClickAway
  }


defConfig :: Config m
defConfig = Config [] ClosesOnClickAway


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


instance (Consideration ConsideredChoice p, PickToSelected p) => Selection Dropdown p where
  select  (Dropdown c t) x = close $ Dropdown (select c x) t
  unselected = unselected . _considered
  selected   = selected . _considered
  withOptions x xs  = Dropdown (x `withOptions` xs) mempty
  retain (Dropdown c t) (Dropdown c' t') = Dropdown (retain c c') (t <> t')


instance (Consideration ConsideredChoice p, Deselection ConsideredChoice p)
    => Deselection Dropdown p where
  noselection xs = Dropdown (noselection xs) mempty
  deselect (Dropdown c t) = close $ Dropdown (deselect c) t


instance (Consideration ConsideredChoice p, PickToConsidered p) => Consideration Dropdown p where
  consider  x (Dropdown c t) = Dropdown (consider x c) t
  choose (Dropdown c t) = Dropdown (choose c) t
  choice (Dropdown c _) = choice c
  considered (Dropdown c _) = considered c
  shrug (Dropdown c xs) = Dropdown (shrug c) xs


data Theme m p b = Theme
    { _wrapper :: forall a . [Html m a]   ->  Html m a
    , _header  :: forall a . Selected p b -> [Html m a]
    , _list    :: forall a . [Html m a]   ->  Html m a
    , _item    :: forall a . b            ->  Html m a
    }


semantic :: Present b => Present (Selected p b) => Dropdown p b -> Theme m p b
semantic Dropdown {..} = Theme
  { _wrapper = div
    [ class' [ ("dropdown", True)
             , ("ui",       True)
             , ("active", _toggle == Open) ]
    ]
  , _header  = \cs ->
    [ div [ class' "text" ] (present cs)
    , i' [ class' ["dropdown", "icon"] ]
    ]
  , _list    = div
    [ class' [ "menu"
             , "transition" ]
    ]
  , _item    = div [ class' "item" ] . present
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
act x = open x


dropdown ::
  ( Considered p ~ Maybe
  , Consideration Dropdown p
  , Consideration ConsideredChoice p
  , Ord a
  ) => (Dropdown p a -> Theme m p a)
    -> Config m -> Dropdown p a -> Html m (Dropdown p a)
dropdown toTheme Config {..} x =
  let
    Theme {..} = toTheme x
    ifClickAway = case _clickAway of
      ClosesOnClickAway    -> -- [ onClickAway close ]
                              []  -- TODO: seems 'onClick' and 'onClickAway'
                                  --       are mutually exclusive?
      StaysOpenOnClickAway -> []

  in injectProps
  ([onKeyup $ \case
    Enter     -> act
    UpArrow   -> considerPrev
    DownArrow -> considerNext
    _         -> id
  , onClick act
  , tabbable
  ] ++ ifClickAway ++ _attrs) . _wrapper $
  _header (selected x) ++
  [ _list $ (\y -> injectProps
    [ onMouseover (consider' y)
    , onFocus     (consider' y)
    , tabbable
    ] . _item $ y) <$> toList (unselected x)
  ]

#ifdef TESTING
instance (Ord a, Arbitrary a, Arbitrary (ConsideredChoice p a)) => Arbitrary (Dropdown p a) where
  arbitrary = Dropdown <$> arbitrary <*> arbitrary
#endif
