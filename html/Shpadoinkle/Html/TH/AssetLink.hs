module Shpadoinkle.Html.TH.AssetLink where


import           Control.Monad              (unless)
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL), Q,
                                             runIO)
import           Prelude                    hiding (readFile)
import           System.Directory           (doesFileExist)
import           System.Exit                (ExitCode (..))
import           System.Process             (readProcessWithExitCode)


assetLink :: FilePath -> Q Exp
assetLink = assetLinkWithBase ""


assetLinkWithBase :: FilePath -> FilePath -> Q Exp
assetLinkWithBase base fp' = runIO $ do
  let fp = base <> fp'

  exists <- doesFileExist $ "." <> fp
  unless exists . fail $ "No asset found at " <> fp

  out@(exit, hash, _) <- readProcessWithExitCode "sha1sum" ["." <> fp] ""
  case exit of
    ExitSuccess   -> return . LitE . StringL $ fp <> "?_=" <> take 20 hash
    ExitFailure _ -> fail $ show out
