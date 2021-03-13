{-# LANGUAGE LambdaCase #-}


module Shpadoinkle.Template.TH where


import           Data.Text                  (Text, cons, unpack)
import           Data.Text.IO
import           Language.Haskell.TH.Syntax
import           Prelude                    hiding (readFile)
import           Text.HTML.Parser           (Attr (..), Token (..), parseTokens)


embedHtml :: FilePath -> Q Exp
embedHtml path = do
  ts <- runIO $ parseTokens <$> readFile path
  pure . ListE $ tokenToExp ts


tokenToExp :: [Token] -> [Exp]
tokenToExp =
  let h    = UnboundVarE $ mkName "h"
      text = UnboundVarE $ mkName "text" in \case
  TagOpen tn attrs:ts ->
    let attrs' = ListE $ attrToExp <$> attrs
        name = asText tn
        (children, siblings) = break (\case TagClose tn' | tn' == tn -> True; _ -> False) ts
    in AppE (AppE (AppE h name) attrs') (ListE $ tokenToExp children) : tokenToExp (drop 1 siblings)
  TagSelfClose tn attrs:ts ->
    let attrs' = ListE $ attrToExp <$> attrs
        name = asText tn
    in AppE (AppE (AppE h name) attrs') (ListE []) : tokenToExp ts
  TagClose _:ts -> tokenToExp ts
  ContentText content:ts ->
    let content' = asText content
    in AppE text content' : tokenToExp ts
  ContentChar char:ts ->
    let char' = asText $ cons char mempty
     in AppE text char' : tokenToExp ts
  Comment _:ts -> tokenToExp ts
  Doctype _:ts -> tokenToExp ts
  [] -> []


attrToExp :: Attr -> Exp
attrToExp (Attr name value) = TupE [name', AppE textProp value']
  where textProp = UnboundVarE $ mkName "textProp"
        name'    = asText name
        value'   = asText value


asText :: Text -> Exp
asText = AppE (UnboundVarE $ mkName "pack") . LitE . StringL . unpack
