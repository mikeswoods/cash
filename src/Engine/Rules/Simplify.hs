{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
module Engine.Rules.Simplify
    (symbol
    ,simplify
    ,rootToPower
    )
where

import Engine.Expression
import Engine.Util


-- |
rootToPower :: Expr -> Expr
rootToPower (App Sqrt e) = e :** (num 1 :/ num 2)
rootToPower e            = e


-- Eval
simplEval :: Expr -> Expr
simplEval (N n :+ N m)  = num $ n + m
simplEval (N n :- N m)  = num $ n - m
simplEval (N n :* N m)  = num $ n * m
simplEval (N n :/ N m)  = num $ n / m
simplEval (N n :** N m) = num $ n ** m
simplEval e             = e


-- Arithmetic
simplArithmetic :: Expr -> Expr
simplArithmetic (x :+ N 0) = x
simplArithmetic (N 0 :+ x) = x
simplArithmetic (x :- N 0) = x
simplArithmetic (N 0 :- x) = -x
simplArithmetic (x :* N 1) = x
simplArithmetic (N 1 :* x) = x
simplArithmetic (_ :* N 0) = num 0
simplArithmetic (N 0 :* _) = num 0
simplArithmetic e            = e


-- | Exponents
simplExponents :: Expr -> Expr
simplExponents (e :** N 1)    = e
simplExponents (_ :** N 0)    = num 1
simplExponents (e :** N (-1)) = recip e
simplExponents e@(x :** N n)
  | n < 0     = recip (x ** (num $ abs n))
  | otherwise = e 
simplExponents e@((x :** m) :* (x' :** n))
  | x == x'   = x ** (m + n)
  | otherwise = e
simplExponents e@((x :** m) :/ (x' :** n))
  | x == x'   = x ** (m - n)
  | otherwise = e
simplExponents ((x :** m) :** n) = x ** (m * n)
simplExponents ((x :* y) :** n)  = (x ** n) * (y ** n)
simplExponents ((x :/ y) :** n)  = (x ** n) / (y ** n)
simplExponents e                 = e


-- |
simplLogarithms :: Expr -> Expr
simplLogarithms (App Log ((C E) :** x)) = x
simplLogarithms (App Log (C E))         = num 1
simplLogarithms (App Log (x :* y))      = (log x) + (log y)
simplLogarithms (App Log (x :** y))     = y * (log x)
simplLogarithms (App Log (N 1 :/ x))    = -(log x)
simplLogarithms (App Log (N 1))         = num 0
simplLogarithms (App Exp (App Log x))   = x
simplLogarithms e                       = e


-- |
simplifier :: Expr -> Expr
simplifier e = g e
  where
    fs = [simplEval
         ,simplArithmetic
         ,simplExponents
         ,simplLogarithms
         ]
    g  = foldr (.) id  fs


-- | Recursive simplifier that walks the expression tree
simplRec :: Expr -> Expr
simplRec (e1 :+ e2)  = simplifier (simplRec $ simplifier e1) :+ (simplRec $ simplifier e2)
simplRec (e1 :- e2)  = simplifier (simplRec $ simplifier e1) :- (simplRec $ simplifier e2)
simplRec (e1 :* e2)  = simplifier (simplRec $ simplifier e1) :* (simplRec $ simplifier e2)
simplRec (e1 :/ e2)  = simplifier (simplRec $ simplifier e1) :/ (simplRec $ simplifier e2)
simplRec (e1 :** e2) = simplifier (simplRec $ simplifier e1) :** (simplRec $ simplifier e2)
simplRec e           = simplifier e


-- | Apply the simplifier until we reach a fixed point for the expression
simplify :: Expr -> Expr
simplify = run
  where
    run e = apply e (simplRec e)
    apply e f
      | e == f    = e
      | otherwise = run f

