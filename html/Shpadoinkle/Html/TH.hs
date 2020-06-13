{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}


module Shpadoinkle.Html.TH where


import qualified Data.Char           as Char
import qualified Data.Text
import qualified GHC.Base
import           Language.Haskell.TH


import           Shpadoinkle         hiding (name)
import           Shpadoinkle.Functor


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
    a  = VarT $ mkName "a"
    e  = VarT $ mkName "e"

  in return

    [ SigD name (ForallT [] [ AppT (AppT (ConT ''Shpadoinkle.Propish) p) e ]
      (AppT (AppT ArrowT (AppT e a))
        (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
          (AppT p a))))

    , FunD name  [Clause [] (NormalB $ AppE (VarE l)  (LitE $ StringL evt)) []]


    , SigD name'
      (ForallT []
        [ AppT (ConT ''GHC.Base.Applicative) m
        , AppT (AppT (ConT ''Shpadoinkle.Propish) p) e ]
        (AppT (AppT ArrowT (AppT e a))
          (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
           (AppT p a))))

    , FunD name' [Clause [] (NormalB $ AppE (VarE l') (LitE $ StringL evt)) []]
    ]


mkProp :: Name -> String -> String -> Q [Dec]
mkProp type' l' name' = let

    name = reverse $ case reverse name' of
             '\'':rs -> rs
             rs      -> rs
    a = VarT $ mkName "a"
    p = VarT $ mkName "p"
    e = VarT $ mkName "e"
    l = mkName l'
    n = mkName name'

  in return

    [ SigD n (ForallT [] [AppT (AppT (ConT ''Shpadoinkle.Propish) p) e]
      (AppT (AppT ArrowT (ConT type'))
       (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
        (AppT p a))))

    , ValD (VarP n) (NormalB (AppE (VarE l) (LitE (StringL name)))) []
    ]


mkTextProp :: String -> Q [Dec]
mkTextProp = mkProp ''Data.Text.Text "textProperty"


mkBoolProp :: String -> Q [Dec]
mkBoolProp = mkProp ''Bool "flagProperty"


mkIntProp :: String -> Q [Dec]
mkIntProp = mkProp ''Int "textProperty"


mkElement :: String -> Q [Dec]
mkElement name = let

    n   = mkName name
    n'  = mkName $ name ++ "'"
    n_  = mkName $ name ++  "_"
    n'_ = mkName $ name ++ "'_"
    h   = VarT $ mkName "h"
    p   = VarT $ mkName "p"
    e   = VarT $ mkName "e"
    a   = VarT $ mkName "a"
    l   = mkName "htmlNode"

  in return

    [ SigD n (ForallT [] [ AppT (AppT (ConT ''Shpadoinkle.Htmlish) h) p
                         , AppT (AppT (ConT ''Shpadoinkle.Propish) p) e ]
      (AppT (AppT ArrowT (AppT ListT (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
                                      (AppT p a))))
        (AppT (AppT ArrowT (AppT ListT (AppT h a))
                (AppT h a))))

    , ValD (VarP n) (NormalB (AppE (VarE l) (LitE (StringL name)))) []


    , SigD n_ (ForallT [] [Appt (AppT (ConT ''Shpadoinkle.Htmlish) h) p]
               (AppT (AppT ArrowT (AppT ListT (AppT h a)))
                (AppT h a)))

    , ValD (VarP n_) (NormalB (AppE (VarE n) (ListE []))) []


    , SigD n' (ForallT [] [ AppT (AppT (ConT ''Shpadoinkle.Htmlish) h) p
                          , AppT (AppT (ConT ''Shpadoinkle.Propish) p) e ]
      (AppT (AppT ArrowT (AppT ListT (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text)) (AppT p a))))
       (AppT h a)))

    , ValD (VarP n') (NormalB (AppE (AppE (VarE (mkName "flip")) (VarE n)) (ListE []))) []


    , SigD n'_ (ForallT [] [AppT (AppT (ConT ''Shpadoinkle.Htmlish) h) p] (AppT h a))

    , ValD (VarP n'_) (NormalB (AppE (AppE (VarE n) (ListE [])) (ListE []))) []

    ]
