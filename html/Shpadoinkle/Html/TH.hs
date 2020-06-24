{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}


module Shpadoinkle.Html.TH where


import qualified Data.Char           as Char
import qualified Data.Text
import           Language.Haskell.TH


import           Shpadoinkle         hiding (h, name)


capitalized :: String -> String
capitalized (c:cs) = Char.toUpper c : fmap Char.toLower cs
capitalized []     = []


mkEventDSL :: String -> Q [Dec]
mkEventDSL evt = let

    onevt = "on" ++ capitalized evt
    name   = mkName onevt
    nameE  = mkName $ onevt ++ "E"
    nameM  = mkName $ onevt ++ "M"
    nameM_ = mkName $ onevt ++ "M_"
    l   = mkName "listen"
    lE  = mkName "listenE"
    lM  = mkName "listenM"
    lM_ = mkName "listenM_"
    m   = VarT $ mkName "m"
    a   = VarT $ mkName "a"
    e   = VarT $ mkName "e"
    p   = VarT $ mkName "p"

  in return

    [ SigD nameM (ForallT [] [ AppT (ConT ''Prelude.Monad) m ]
      (AppT (AppT ArrowT (AppT m ((AppT (AppT ArrowT a) a))))
        (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
         (AppT (AppT (ConT ''Shpadoinkle.PropM) m) a))))

    , FunD nameM  [Clause [] (NormalB $ AppE (VarE lM)  (LitE $ StringL evt)) []]


    , SigD nameM_ (ForallT [] [ AppT (ConT ''Prelude.Monad) m ]
      (AppT (AppT ArrowT (AppT m (ConT ''())))
       (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
         (AppT (AppT (ConT ''Shpadoinkle.PropM) m) a))))

    , FunD nameM_ [Clause [] (NormalB $ AppE (VarE lM_) (LitE $ StringL evt)) []]


    , SigD nameE
      (ForallT [] [ AppT (AppT (ConT ''Shpadoinkle.IsProp) p) e ]
        (AppT (AppT ArrowT (AppT e a))
         (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
           (AppT p a))))

    , FunD nameE [Clause [] (NormalB $ AppE (VarE lE) (LitE $ StringL evt)) []]

    , SigD name
      (ForallT []
        [ AppT (AppT (ConT ''Shpadoinkle.IsProp) p) e ]
        (AppT (AppT ArrowT a)
          (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
           (AppT p a))))

    , FunD name [Clause [] (NormalB $ AppE (VarE l) (LitE $ StringL evt)) []]
    ]


mkProp :: Name -> String -> String -> Q [Dec]
mkProp typ lStr name' = let

    name = reverse $ case reverse name' of
             '\'':rs -> rs
             rs      -> rs
    a = VarT $ mkName "a"
    p = VarT $ mkName "p"
    e = VarT $ mkName "e"
    l = mkName lStr
    n = mkName name'

  in return

    [ SigD n (ForallT [] [AppT (AppT (ConT ''Shpadoinkle.IsProp) p) e]
      (AppT (AppT ArrowT (ConT typ))
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

    raw = filter (not . (== '\'')) name
    n   = mkName name
    n'  = mkName $ name ++ "'"
    n_  = mkName $ name ++  "_"
    n'_ = mkName $ name ++ "'_"
    h   = VarT $ mkName "h"
    p   = VarT $ mkName "p"
    a   = VarT $ mkName "a"
    l   = mkName "h"

  in return

    [ SigD n (ForallT [] [ AppT (AppT (ConT ''Shpadoinkle.IsHtml) h) p ]
      (AppT (AppT ArrowT (AppT ListT (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
                                      (AppT p a))))
        (AppT (AppT ArrowT (AppT ListT (AppT h a)))
                (AppT h a))))

    , ValD (VarP n) (NormalB (AppE (VarE l) (LitE (StringL raw)))) []


    , SigD n_ (ForallT [] [AppT (AppT (ConT ''Shpadoinkle.IsHtml) h) p]
               (AppT (AppT ArrowT (AppT ListT (AppT h a)))
                (AppT h a)))

    , ValD (VarP n_) (NormalB (AppE (VarE n) (ListE []))) []


    , SigD n' (ForallT [] [ AppT (AppT (ConT ''Shpadoinkle.IsHtml) h) p ]
      (AppT (AppT ArrowT (AppT ListT (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text)) (AppT p a))))
       (AppT h a)))

    , ValD (VarP n') (NormalB (AppE (AppE (VarE (mkName "flip")) (VarE n)) (ListE []))) []


    , SigD n'_ (ForallT [] [AppT (AppT (ConT ''Shpadoinkle.IsHtml) h) p] (AppT h a))

    , ValD (VarP n'_) (NormalB (AppE (AppE (VarE n) (ListE [])) (ListE []))) []

    ]
