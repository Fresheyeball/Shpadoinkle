{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Shpadoinkle.Html.TH where


import qualified Data.Char           as Char
import qualified Data.Text
import           Language.Haskell.TH (Body (NormalB), Clause (Clause),
                                      Dec (FunD, SigD, ValD),
                                      Exp (AppE, InfixE, ListE, LitE, UnboundVarE, VarE),
                                      Lit (StringL), Name, Pat (VarP), Q,
                                      Type (AppT, ArrowT, ConT, ForallT, ListT, TupleT, VarT),
                                      mkName)


import           Shpadoinkle         (Continuation, Html, Prop, causes, impur,
                                      pur)


{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


capitalized :: String -> String
capitalized (c:cs) = Char.toUpper c : cs
capitalized []     = []



mkEventDSL :: String -> Q [Dec]
mkEventDSL evt = let

    onevt = "on" ++ capitalized evt
    name   = mkName onevt
    nameC  = mkName $ onevt ++ "C"
    nameM  = mkName $ onevt ++ "M"
    nameM_ = mkName $ onevt ++ "M_"
    l   = mkName "listen"
    lC  = mkName "listenC"
    lM  = mkName "listenM"
    lM_ = mkName "listenM_"
    m   = VarT $ mkName "m"
    a   = VarT $ mkName "a"

  in return

    [ SigD nameM (ForallT [] [ AppT (ConT ''Prelude.Monad) m ]
      (AppT (AppT ArrowT (AppT m (AppT (AppT ArrowT a) a)))
        (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
         (AppT (AppT (ConT ''Shpadoinkle.Prop) m) a))))

    , FunD nameM  [Clause [] (NormalB $ AppE (VarE lM)  (LitE $ StringL evt)) []]


    , SigD nameM_ (ForallT [] [ AppT (ConT ''Prelude.Monad) m ]
      (AppT (AppT ArrowT (AppT m (ConT ''())))
       (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
         (AppT (AppT (ConT ''Shpadoinkle.Prop) m) a))))

    , FunD nameM_ [Clause [] (NormalB $ AppE (VarE lM_) (LitE $ StringL evt)) []]


    , SigD nameC
      (ForallT [] [ ]
        (AppT (AppT ArrowT (AppT (AppT (ConT ''Shpadoinkle.Continuation) m) a))
         (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
           (AppT (AppT (ConT ''Shpadoinkle.Prop) m) a))))

    , FunD nameC [Clause [] (NormalB $ AppE (VarE lC) (LitE $ StringL evt)) []]

    , SigD name
      (ForallT []
        []
        (AppT (AppT ArrowT (AppT (AppT ArrowT a) a))
          (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
           (AppT (AppT (ConT ''Shpadoinkle.Prop) m) a))))

    , FunD name [Clause [] (NormalB $ AppE (VarE l) (LitE $ StringL evt)) []]
    ]


mkEventVariants :: String -> Q [Dec]
mkEventVariants evt = let
    onevt = "on" ++ capitalized evt
    name   = mkName onevt
    nameC  = mkName $ onevt ++ "C"
    nameM  = mkName $ onevt ++ "M"
    nameM_ = mkName $ onevt ++ "M_"

    m   = VarT $ mkName "m"
    a   = VarT $ mkName "a"

  in return
    [ SigD nameM (ForallT [] [ AppT (ConT ''Prelude.Monad) m ]
      (AppT (AppT ArrowT (AppT m (AppT (AppT ArrowT a) a)))
        (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
         (AppT (AppT (ConT ''Shpadoinkle.Prop) m) a))))

    , FunD nameM  [ Clause [] (NormalB $ AppE (AppE (VarE '(Prelude..)) (VarE nameC)) (VarE 'Shpadoinkle.impur)) []]

    , SigD nameM_ (ForallT [] [ AppT (ConT ''Prelude.Monad) m ]
      (AppT (AppT ArrowT (AppT m (ConT ''())))
       (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
         (AppT (AppT (ConT ''Shpadoinkle.Prop) m) a))))

    , FunD nameM_ [ Clause [] (NormalB $ AppE (AppE (VarE '(Prelude..)) (VarE nameC)) (VarE 'Shpadoinkle.causes)) []]

    , SigD name
      (ForallT []
        []
        (AppT (AppT ArrowT (AppT (AppT ArrowT a) a))
          (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
           (AppT (AppT (ConT ''Shpadoinkle.Prop) m) a))))

    , FunD name [ Clause [] (NormalB $ AppE (AppE (VarE '(Prelude..)) (VarE nameC)) (VarE 'Shpadoinkle.pur)) []]

    ]


mkEventVariantsAfforded :: String -> Name -> Q [Dec]
mkEventVariantsAfforded evt afford = let
    onevt = "on" ++ capitalized evt
    name   = mkName onevt
    nameC  = mkName $ onevt ++ "C"
    nameM  = mkName $ onevt ++ "M"
    nameM_ = mkName $ onevt ++ "M_"
    f   = mkName "f"

    m   = VarT $ mkName "m"
    a   = VarT $ mkName "a"

  in return
    [ SigD nameM (ForallT [] [ AppT (ConT ''Prelude.Monad) m ]
         (AppT (AppT ArrowT (AppT (AppT ArrowT (ConT afford)) (AppT m (AppT (AppT ArrowT a) a))))
           (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
             (AppT (AppT (ConT ''Shpadoinkle.Prop) m) a))))

    , FunD nameM [Clause [VarP f] (NormalB (AppE (UnboundVarE nameC) (InfixE (Just (UnboundVarE 'Shpadoinkle.impur)) (VarE '(Prelude..)) (Just (VarE f))))) []]

    , SigD nameM_ (ForallT [] [ AppT (ConT ''Prelude.Monad) m ]
         (AppT (AppT ArrowT (AppT (AppT ArrowT (ConT afford)) (AppT m (ConT ''()))))
           (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
             (AppT (AppT (ConT ''Shpadoinkle.Prop) m) a))))

    , FunD nameM_ [Clause [VarP f] (NormalB (AppE (UnboundVarE nameC) (InfixE (Just (UnboundVarE 'Shpadoinkle.causes)) (VarE '(Prelude..)) (Just (VarE f))))) []]

    , SigD name
          (AppT (AppT ArrowT (AppT (AppT ArrowT (ConT afford)) (AppT (AppT ArrowT a) a)))
            (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
              (AppT (AppT (ConT ''Shpadoinkle.Prop) m) a)))

    , FunD name [Clause [VarP f] (NormalB (AppE (UnboundVarE nameC) (InfixE (Just (UnboundVarE 'Shpadoinkle.pur)) (VarE '(Prelude..)) (Just (VarE f))))) []]

    ]


mkProp :: Name -> String -> String -> Q [Dec]
mkProp typ lStr name' = let

    name = reverse $ case reverse name' of
             '\'':rs -> rs
             rs      -> rs
    a = VarT $ mkName "a"
    m = VarT $ mkName "m"
    l = mkName lStr
    n = mkName name'

  in return

    [ SigD n (ForallT [] []
      (AppT (AppT ArrowT (ConT typ))
       (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
        (AppT (AppT (ConT ''Shpadoinkle.Prop) m) a))))

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

    raw = filter (/= '\'') name
    n   = mkName name
    n'  = mkName $ name ++ "'"
    n_  = mkName $ name ++  "_"
    n'_ = mkName $ name ++ "'_"
    m   = VarT $ mkName "m"
    a   = VarT $ mkName "a"
    l   = mkName "h"

  in return

    [ SigD n
      (AppT (AppT ArrowT (AppT ListT (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text))
                                      (AppT (AppT (ConT ''Shpadoinkle.Prop) m) a))))
        (AppT (AppT ArrowT (AppT ListT (AppT (AppT (ConT ''Shpadoinkle.Html) m) a)))
                (AppT (AppT (ConT ''Shpadoinkle.Html) m) a)))

    , ValD (VarP n) (NormalB (AppE (VarE l) (LitE (StringL raw)))) []


    , SigD n_
      (AppT (AppT ArrowT (AppT ListT (AppT (AppT (ConT ''Shpadoinkle.Html) m) a)))
       (AppT (AppT (ConT ''Shpadoinkle.Html) m) a))

    , ValD (VarP n_) (NormalB (AppE (VarE n) (ListE []))) []


    , SigD n'
      (AppT (AppT ArrowT (AppT ListT (AppT (AppT (TupleT 2) (ConT ''Data.Text.Text)) (AppT (AppT (ConT ''Shpadoinkle.Prop) m) a))))
       (AppT (AppT (ConT ''Shpadoinkle.Html) m) a))

    , ValD (VarP n') (NormalB (AppE (AppE (VarE (mkName "flip")) (VarE n)) (ListE []))) []


    , SigD n'_ (AppT (AppT (ConT ''Shpadoinkle.Html) m) a)

    , ValD (VarP n'_) (NormalB (AppE (AppE (VarE n) (ListE [])) (ListE []))) []

    ]
