{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}


module Shpadoinkle.Html.TH.CSS
  ( extractNamespace
  , textProperty'
  ) where


import           Control.Compactable
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy.Char8  as BS
import           Data.Containers.ListUtils   (nubOrd)
import qualified Data.Set                    as Set
import           Data.String                 (IsString)
import           Data.Text                   (Text)
import           Language.Haskell.TH.Syntax

#ifdef ghcjs_HOST_OS
import           Data.Text.Encoding
import           Language.Javascript.JSaddle
import           System.IO.Unsafe            (unsafePerformIO)
#else
import           Text.Regex.PCRE
#endif

import           Shpadoinkle                 (IsProp)
import           Shpadoinkle.Html            (ClassList (..))
import           Shpadoinkle.Html.Property   (textProperty')


extractNamespace :: FilePath -> Q [Dec]
extractNamespace fp = do
  css <- runIO $ BS.readFile fp
  return . split . nubOrd $ getAll css


getAll :: ByteString -> [ByteString]
#ifdef ghcjs_HOST_OS
getAll css = unsafePerformIO $ do
  matches <- eval . decodeUtf8 . BS.toStrict $
    "Array.from(`" <> css <> "`.match(/" <> selectors <> "/g))"
  maybe [] (fmap $ BS.fromStrict . encodeUtf8) <$> fromJSVal matches
#else
getAll css = getAllTextMatches $ css =~ (selectors @ByteString)
#endif
{-# NOINLINE getAll #-}



split :: [ByteString] -> [Dec]
split ss = (toClassDec =<< classes) <> (toIdDec =<< ids) where
  (classes, ids) = fforEither ss $ \selector -> case BS.uncons selector of
    Just ('.', class') -> Left class'
    Just ('#', id')    -> Right id'
    _                  -> error "Selector found that is not and id or class"


toIdDec :: ByteString -> [Dec]
toIdDec name = let
    a = VarT $ mkName "a"
    p = VarT $ mkName "p"
    e = VarT $ mkName "e"
    l = mkName "textProperty'"
    name' = BS.unpack name
    n = mkName $ "id'" <> sanitize name'
  in
    [ SigD n (ForallT [] [AppT (AppT (ConT ''Shpadoinkle.IsProp) p) e]
      ((AppT (AppT (TupleT 2) (ConT ''Data.Text.Text)) (AppT p a))))
    , ValD (VarP n) (NormalB (AppE (AppE (VarE l) (LitE (StringL "id"))) (LitE (StringL name')))) []
    ]


toClassDec :: ByteString -> [Dec]
toClassDec n' = let n = mkName . sanitize $ BS.unpack n' in
  [ SigD n (ConT ''ClassList)
  , ValD (VarP n) (NormalB (AppE (ConE 'ClassList)
      (AppE (VarE 'Set.singleton) (LitE (StringL $ BS.unpack n'))))) []
  ]


sanitize :: String -> String
sanitize = (=<<) $ \case
  '/' -> "''"
  '-' -> "_"
  ':' -> "'"
  '>' -> "GT"
  x   -> pure x


selectors :: IsString s => s
selectors = "(#|\\.)-?[_a-zA-Z]+[_a-zA-Z0-9-]*(?=[^}]*\\{)"
