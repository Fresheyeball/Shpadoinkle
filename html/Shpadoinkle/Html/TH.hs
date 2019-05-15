{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}


module Shpadoinkle.Html.TH where


import qualified Data.Char           as Char
import qualified Data.Text
import qualified GHC.Base
import           Language.Haskell.TH


import           Shpadoinkle         hiding (name)


capitalized :: String -> String
capitalized (c:cs) = Char.toUpper c : fmap Char.toLower cs
capitalized []     = []


mkEventDSL :: String -> Q [Dec]
mkEventDSL evt = let

    onevt = "on" ++ capitalized evt
    name' = mkName onevt
    name  = mkName $ onevt ++ "'"
    l  = mkName "listen"
    l' = mkName "listen'"
    m  = VarT $ mkName "m"
    o  = VarT $ mkName "o"

  in return

    [ SigD name (ForallT [] []
      (AppT (AppT ArrowT (AppT m o)) (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
      (AppT (AppT (ConT ''Shpadoinkle.Prop) m) o))))

    , FunD name  [Clause [] (NormalB $ AppE (VarE l)  (LitE $ StringL evt)) []]


    , SigD name'
      (ForallT []
        [AppT (ConT ''GHC.Base.Applicative) m]
        (AppT (AppT ArrowT o) (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
          (AppT (AppT (ConT ''Shpadoinkle.Prop) m) o))))

    , FunD name' [Clause [] (NormalB $ AppE (VarE l') (LitE $ StringL evt)) []]
    ]


mkTextProp :: String -> Q [Dec]
mkTextProp name' = let


    name = reverse $ case reverse name' of
             '\'':rs -> rs
             rs      -> rs
    m = VarT $ mkName "m"
    o = VarT $ mkName "o"
    l = mkName "textProperty"
    n = mkName name'

  in return

    [ SigD n (ForallT [] []
      (AppT (AppT ArrowT (ConT ''Data.Text.Text))
      (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
      (AppT (AppT (ConT ''Shpadoinkle.Prop) m) o))))

    , ValD (VarP n) (NormalB (AppE (VarE l) (LitE (StringL name)))) []
    ]


mkElement :: String -> Q [Dec]
mkElement name = let

    n = mkName name
    n' = mkName $ name ++ "'"
    n_ = mkName $ name ++ "_"
    n_' = mkName $ name ++ "_'"
    m = VarT $ mkName "m"
    o = VarT $ mkName "o"
    l = mkName "h"

  in return

    [ SigD n (ForallT [] []
      (AppT (AppT ArrowT (AppT ListT (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
      (AppT (AppT (ConT ''Shpadoinkle.Prop) m) o))))
      (AppT (AppT ArrowT (AppT ListT (AppT (AppT (ConT ''Shpadoinkle.Html) m) o)))
      (AppT (AppT (ConT ''Shpadoinkle.Html) m) o))))

    , ValD (VarP n) (NormalB (AppE (VarE l) (LitE (StringL name)))) []


    , SigD n_ (ForallT [] []
      (AppT (AppT ArrowT (AppT ListT (AppT (AppT (ConT ''Shpadoinkle.Html) m) o)))
      (AppT (AppT (ConT ''Shpadoinkle.Html) m) o)))

    , ValD (VarP n_) (NormalB (AppE (VarE n) (ListE []))) []


    , SigD n' (ForallT [] []
      (AppT (AppT ArrowT (AppT ListT (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
      (AppT (AppT (ConT ''Shpadoinkle.Prop) m) o))))
      (AppT (AppT (ConT ''Shpadoinkle.Html) m) o)))

    , ValD (VarP n') (NormalB (AppE (AppE (VarE (mkName "flip")) (VarE n)) (ListE []))) []


    , SigD n_' (ForallT [] []
      (AppT (AppT (ConT ''Shpadoinkle.Html) m) o))

    , ValD (VarP n_')
      (NormalB (AppE (AppE (VarE n) (ListE [])) (ListE []))) []

    ]