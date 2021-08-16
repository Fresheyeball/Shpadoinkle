{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Shpadoinkle.Template.TH where


import           Control.Monad              (unless, when)
import           Data.Text                  (Text, cons, pack, replace, unpack)
import           Data.Text.IO
import           Language.Haskell.TH.Syntax
import           Prelude                    hiding (head, null, readFile, tail)
import           System.Directory           (doesFileExist, removeFile)
import           System.Exit                (ExitCode (..))
import           System.Process             (proc,
                                             readCreateProcessWithExitCode)
import           Text.HTML.Parser           (Attr (..), Token (..), parseTokens)



data CleanUp = CleanUp | LeaveFile deriving Eq


embedAsciidoc :: FilePath -> Q Exp
embedAsciidoc asciiPath = do
  let htmlPath = unpack $ replace ".adoc" ".html" $ pack asciiPath
  out@(exit, _, _) <- runIO $ do
    doesAscii <- doesFileExist asciiPath
    _ <- unless doesAscii . fail $ "Document not found at " <> asciiPath
    doesHtml <- doesFileExist htmlPath
    when doesHtml $ removeFile htmlPath
    readCreateProcessWithExitCode (proc "asciidoctor" [ "-s", asciiPath ]) ""
  case exit of
    ExitSuccess   -> embedHtml' CleanUp htmlPath
    ExitFailure _ -> fail $ show out


embedHtml :: FilePath -> Q Exp
embedHtml = embedHtml' LeaveFile


embedHtml' :: CleanUp -> FilePath -> Q Exp
embedHtml' clean htmlPath = do
  ts  <- runIO $ do
    doesHtml <- doesFileExist htmlPath
    _ <- unless doesHtml . fail $ "Html not found at " <> htmlPath
    ts' <- parseTokens <$> readFile htmlPath
    when (clean == CleanUp) $ removeFile htmlPath
    return ts'
  pure . ListE $ tokenToExp ts


breakClosing :: Text -> [Token] -> ([Token],[Token])
breakClosing tn = go (0 :: Int)
  where

  sameTag = \case
    TagOpen  tn' _ | tn' == tn -> True
    TagClose tn'   | tn' == tn -> True
    _                          -> False

  go depth ts = case break sameTag ts of

    -- closing tag at the top level, we are done
    (before, t@(TagClose tn':_))
      | tn' == tn && depth == 0 -> (before, t)

    -- closing tag found at a deeper level, collect and decrement
    (before, t@(TagClose tn'):more)
      | tn' == tn -> let (before', rest') = go (depth - 1) more
                     in (before <> [t] <> before', rest')

    -- sibling opening tag found, decend
    (before, t@(TagOpen tn' _):children)
      | tn == tn' -> let (before', rest') = go (depth + 1) children
                     in (before <> [t] <> before', rest')

    x -> x



tokenToExp :: [Token] -> [Exp]
tokenToExp =
  let h    = UnboundVarE $ mkName "h"
      text = UnboundVarE $ mkName "text" in \case
  TagOpen "hr" attrs:ts -> tokenToExp $ TagSelfClose "hr" attrs:ts
  TagOpen tn attrs:ts ->
    let attrs' = ListE $ attrToExp <$> attrs
        name = asText tn
        (children, siblings) = breakClosing tn ts
    in AppE (AppE (AppE h name) attrs') (ListE $ tokenToExp children) : tokenToExp siblings
  TagSelfClose tn attrs:ts ->
    let attrs' = ListE $ attrToExp <$> attrs
        name = asText tn
    in AppE (AppE (AppE h name) attrs') (ListE []) : tokenToExp ts
  TagClose _:ts -> tokenToExp ts
  ContentText content:ts ->
    if content == "\56608"
    then tokenToExp ts else let content' = asText content
                             in AppE text content' : tokenToExp ts
  ContentChar char:ts ->
    let char' = asText $ cons char mempty
     in AppE text char' : tokenToExp ts
  Comment _:ts -> tokenToExp ts
  Doctype _:ts -> tokenToExp ts
  [] -> []


attrToExp :: Attr -> Exp
attrToExp (Attr name value) = TupE [name', AppE textProp value']
  where
  textProp = UnboundVarE $ mkName "textProp"
  name'    = asText $ case name of
    "class" -> "className"
    _       -> name
  value'   = asText value


asText :: Text -> Exp
asText = AppE (UnboundVarE $ mkName "pack") . LitE . StringL . unpack
