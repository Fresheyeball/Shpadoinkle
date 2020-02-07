{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}


module Shpadoinkle.Widgets.Types.Search where


import           Data.Aeson
import           Data.Foldable     as F
import           Data.List         (sort)
import           Data.Maybe        (mapMaybe)
import           Data.String
import           Data.Text
import           Data.Text.Metrics
import           GHC.Generics


newtype Search = Search { unSearch :: Text }
  deriving newtype (Eq, Ord, Show, Read, IsString, Semigroup, Monoid, ToJSON, FromJSON)
  deriving stock Generic


data Levenshtiened a = Levenshtiened { _distance :: Int, _unLevenshtiened :: a } deriving Eq
instance Eq       a => Ord      (Levenshtiened a) where
  compare (Levenshtiened x _) (Levenshtiened y _) = compare x y


mkLevenshtiened :: Text -> Search -> a -> Levenshtiened a
mkLevenshtiened  t (Search s) x = Levenshtiened (levenshtein (strip s) (strip t)) x


forgivingly :: Search -> Text -> Bool
forgivingly (Search (strip -> "")) _ = True
forgivingly (Search s) haystack = Prelude.all test . splitOn " " $ strip s
  where test ""     = False
        test needle = forgive needle `isInfixOf` forgive haystack
        forgive = toLower . strip


concatFuzzy :: [a -> Text] -> a -> Text
concatFuzzy = F.foldl' (\f g a -> f a <> " " <> g a) (const "")


search :: Ord a => [a -> Text] -> Search -> [a] -> [Levenshtiened a]
search toChunks s = sort .
  mapMaybe (\x -> let hay = concatFuzzy toChunks x
                  in if forgivingly s hay
                     then Just $ mkLevenshtiened hay s x
                     else Nothing
           )
