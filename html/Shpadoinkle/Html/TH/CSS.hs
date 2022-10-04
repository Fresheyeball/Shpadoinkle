{-# LANGUAGE CPP                   #-}
{-# LANGUAGE JavaScriptFFI         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -ddump-splices #-}


module Shpadoinkle.Html.TH.CSS
  ( extractNamespace
  , textProperty'
  ) where


import           Control.Compactable
import           Data.ByteString.Lazy       as BS (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char                  (toLower)
import           Data.Containers.ListUtils  (nubOrd)
import qualified Data.Set                   as Set
import           Data.String                (IsString)
import           Data.Text                  (Text)
import           Language.Haskell.TH.Syntax

#ifdef ghcjs_HOST_OS
import           Data.Text.Encoding
import           GHCJS.Marshal.Pure
import           GHCJS.Types                as T
import           Shpadoinkle.JSFFI          (JSArray, JSM, fromJSValUnsafe,
                                             jsArrayToList, toTextLax)
import           System.IO.Unsafe           (unsafePerformIO)
#else
import           Text.Regex.PCRE
#endif

import           Shpadoinkle                (Prop)
import           Shpadoinkle.Html           (ClassList (..))
import           Shpadoinkle.Html.Property  (textProperty')


extractNamespace :: FilePath -> Q [Dec]
extractNamespace fp = do
  css <- runIO $ BS.readFile fp
  return . split . nubOrd $ getAll css


#ifdef ghcjs_HOST_OS

foreign import javascript unsafe "Array.from($1.match(new RegExp($2, 'g')))"
  js_match :: T.JSString -> T.JSString -> IO JSVal

match :: T.JSString -> T.JSString -> JSM JSArray
match a b = fromJSValUnsafe @JSArray <$> js_match a b


notMempty :: (Eq m, Monoid m) => m -> Maybe m
notMempty x | x == mempty = Nothing
            | otherwise   = Just x


getAll :: ByteString -> [ByteString]
getAll css = unsafePerformIO $ do
  matches <- match (pFromJSVal . pToJSVal $ BS.unpack css) selectors
  matches' <- traverse toTextLax =<< jsArrayToList matches
  pure $ fmapMaybe (notMempty . BS.fromStrict . encodeUtf8) matches'

#else

getAll :: ByteString -> [ByteString]
getAll css = getAllTextMatches $ css =~ selectors @ByteString

#endif


split :: [ByteString] -> [Dec]
split ss = (toClassDec =<< classes) <> (toIdDec =<< ids) where
  (classes, ids) = fforEither ss $ \selector -> case BS.uncons selector of
    Just ('.', class') -> Left class'
    Just ('#', id')    -> Right id'
    _                  -> error "Selector found that is not and id or class"


toIdDec :: ByteString -> [Dec]
toIdDec ""   = []
toIdDec name = let
    a = VarT $ mkName "a"
    m = VarT $ mkName "m"
    l = mkName "textProperty'"
    name' = case BS.unpack name of
              '#':rs -> rs
              rs     -> rs
    n = mkName $ "id'" <> sanitize name'
  in
    [ SigD n
      (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text)) (AppT (AppT (ConT ''Shpadoinkle.Prop) m) a))
    , ValD (VarP n) (NormalB (AppE (AppE (VarE l) (LitE (StringL "id"))) (LitE (StringL name')))) []
    ]


toClassDec :: ByteString -> [Dec]
toClassDec "" = []
toClassDec n' = let
  n = mkName . sanitize $ case BS.unpack n' of
    '.':rs -> rs
    rs     -> rs
  in
  [ SigD n (ConT ''ClassList)
  , ValD (VarP n) (NormalB (AppE (ConE 'ClassList)
      (AppE (VarE 'Set.singleton) (LitE (StringL $ BS.unpack n'))))) []
  ]


sanitize :: String -> String
sanitize = lowerFirst . (=<<) (\case
  '/' -> "''"
  '-' -> "_"
  ':' -> "'"
  '>' -> "GT"
  x   -> pure x)


lowerFirst :: String -> String
lowerFirst (x:xs) = toLower x : xs
lowerFirst x      = x


selectors :: IsString s => s
selectors = "(#|\\.)-?[_a-zA-Z]+[_a-zA-Z0-9-]*(?=[^}]*\\{)"
