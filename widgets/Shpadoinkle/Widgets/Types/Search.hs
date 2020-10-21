{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}


module Shpadoinkle.Widgets.Types.Search where


import           Data.Aeson        (FromJSON, ToJSON)
import           Data.Foldable     as F (Foldable (foldl'))
import           Data.List         (sort)
import           Data.Maybe        (mapMaybe)
import           Data.String       (IsString)
import           Data.Text         (Text, isInfixOf, splitOn, strip, toLower,
                                    unpack)
import           GHC.Generics      (Generic)
import           Text.EditDistance (defaultEditCosts, levenshteinDistance)


newtype Search = Search { unSearch :: Text }
  deriving newtype (Eq, Ord, Show, Read, IsString, Semigroup, Monoid, ToJSON, FromJSON)
  deriving stock Generic


newtype EditDistance = EditDistance { unEditDistance :: Int }
  deriving newtype (Eq, Ord, Show, Read, ToJSON, FromJSON)
  deriving stock Generic


data Levenshtiened a = Levenshtiened { _distance :: !EditDistance, _unLevenshtiened :: a } deriving Eq
instance Eq       a => Ord    (Levenshtiened a) where
  compare (Levenshtiened x _) (Levenshtiened y _) = unEditDistance x `compare` unEditDistance y


mkLevenshtiened :: Text -> Search -> a -> Levenshtiened a
mkLevenshtiened  t (Search s) =
  Levenshtiened . EditDistance $ levenshteinDistance defaultEditCosts (prep s) (prep t)
  where prep = unpack . strip


forgivingly :: Search -> Text -> Bool
forgivingly (Search (strip -> "")) _ = True
forgivingly (Search s) haystack = Prelude.all test . splitOn " " $ strip s
  where test ""     = False
        test needle = forgive needle `isInfixOf` forgive haystack
        forgive = toLower . strip


concatFuzzy :: [a -> Text] -> a -> Text
concatFuzzy = F.foldl' (\f g a -> f a <> " " <> g a) (const "")


fuzzySearch :: Ord a => [a -> Text] -> Search -> [a] -> [a]
fuzzySearch toChunks s = fmap _unLevenshtiened . sort .
  mapMaybe (\x -> let hay = concatFuzzy toChunks x
                  in if forgivingly s hay
                     then Just $ mkLevenshtiened hay s x
                     else Nothing
           )
