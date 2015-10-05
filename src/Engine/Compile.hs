{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}

module Engine.Compile
    (compile1Arg) 
where


import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Engine.Expression


-- |
compile1' :: Expr -> ExpQ
compile1' (N n)             = litE . floatPrimL . toRational $ n
compile1' (C E)           = litE . floatPrimL . toRational $ exp 1.0
compile1' (C Pi)          = litE . floatPrimL . toRational $ pi
compile1' (S (Symbol s))    = varE $ mkName s
compile1' (ApplyOpE (:+) e1 e2)   = infixE (Just $ compile1' e1) (varE plus) (Just $ compile1' e2) where plus = mkName "+"
compile1' (ApplyOpE (:-) e1 e2)  = infixE (Just $ compile1' e1) (varE plus) (Just $ compile1' e2) where plus = mkName "-"
compile1' (ApplyOpE (:*) e1 e2)  = infixE (Just $ compile1' e1) (varE plus) (Just $ compile1' e2) where plus = mkName "*"
compile1' (ApplyOpE (:/) e1 e2) = infixE (Just $ compile1' e1) (varE plus) (Just $ compile1' e2) where plus = mkName "/"
compile1' (ApplyOpE (:^) e1 e2)  = infixE (Just $ compile1' e1) (varE plus) (Just $ compile1' e2) where plus = mkName "**"
compile1' (App fn e)      = undefined


-- |
compile1Arg :: Expr -> ExpQ
compile1Arg e = lamE args body
  where
    args = [varP $ mkName "x"]
    body = compile1' e

--compile :: Floating a => Expr -> (a -> a)
--compile e = $(compile1' e)
