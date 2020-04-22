{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}


module Shpadoinkle.Widgets.Validation where


import           Control.Monad.Except
import           Data.Text
import           Data.Text.Encoding
import           Shpadoinkle.Widgets.Types.Form
import           Text.Email.Validate


nonMEmpty :: (Monoid a, Eq a) => a -> Validated Text a
nonMEmpty x = if x == mempty then throwError "Cannot be empty" else pure x


nonMEmpty' :: (Monoid a, Eq a) => Maybe a -> Validated Text a
nonMEmpty' x = case x of
  Just y  -> nonMEmpty y
  Nothing -> throwError "Cannot be empty"


nonZero :: (Num n, Eq n) => n -> Validated Text n
nonZero n = if n == 0 then throwError "Cannot be zero" else pure n


positive :: (Num n, Ord n) => n -> Validated Text n
positive n = if n < 0 then throwError "Must be positive" else pure n


natural :: (Num n, Ord n) => n -> Validated Text n
natural = nonZero <> positive


negative :: (Num n, Ord n) => n -> Validated Text n
negative n = if n > 0 then throwError "Must be negative" else pure n


between :: (Ord n, Show n) => (n,n) -> n -> Validated Text n
between (sortTup -> (low, high)) n
  | n < low   = throwError $ "Must be greater than " <> pack (show low)
  | n > high  = throwError $ "Must be less than "    <> pack (show high)
  | otherwise = pure n


sortTup :: Ord n => (n,n) -> (n,n)
sortTup (x,y) = if x < y then (x,y) else (y,x)


email :: Text -> Validated Text Text
email i = if isValid (encodeUtf8 i) then pure i else throwError "Not a valid email"
