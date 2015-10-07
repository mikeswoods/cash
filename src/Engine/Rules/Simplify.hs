{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
module Engine.Rules.Simplify
    (symbol
    ,simplify
    ,rootToPower
    )
where

import Engine.Expression


-- |
rootToPower :: Expr -> Expr
rootToPower (App Sqrt e) = e :** (num 1 :/ num 2)
rootToPower e            = e


-- Evaluate numbers under basic arithmetical operations
simplEval :: Expr -> Expr
simplEval (N n :+ N m)  = num $ n + m
simplEval (N n :- N m)  = num $ n - m
simplEval (N n :* N m)  = num $ n * m
simplEval (N n :/ N m)  = num $ n / m
simplEval (N n :** N m) = num $ n ** m
simplEval e             = e


-- Perform basic cancellation steps
simplCancellation :: Expr -> Expr
simplCancellation (x :+ N 0) = x
simplCancellation (N 0 :+ x) = x
simplCancellation (x :- N 0) = x
simplCancellation (N 0 :- x) = -x
simplCancellation (x :* N 1) = x
simplCancellation (N 1 :* x) = x
simplCancellation (_ :* N 0) = num 0
simplCancellation (N 0 :* _) = num 0
simplCancellation e            = e


-- Perform basic arithmetical simplifications
simplArithmetic :: Expr -> Expr
simplArithmetic (a :* (b :+ c)) = (a * b) + (a * c)
simplArithmetic ((b :+ c) :* a) = (a * b) + (a * c)
simplArithmetic (a :* (b :/ c)) = (a * b) / c
simplArithmetic ((b :/ c) :* a) = (a * b) / c
simplArithmetic ((a :/ b) :+ (c :/ d))
  | b == d    = ((a * d) + (b * c)) / (b * d)
  | otherwise = (a + c) / b
simplArithmetic ((a :/ b) :/ c) = a / (b * c)
simplArithmetic (a :/ (b :/ c)) = (a * c) / b
simplArithmetic ((a :/ b) :- (c :/ d))
  | b == d    = (a - c) / b
  | otherwise = ((a * d) - (b * c)) / (b * d)
simplArithmetic e@(((a :* b) :+ (a' :* c)) :/ d)
  | a == a' && a' == d && d /= num 0 = b + c
  | otherwise                        = e
simplArithmetic ((a :/ b) :/ (c :/ d)) = (a * d) / (b * c)
simplArithmetic e = e


-- | Perform simplification based on properties of absolute values
simplAbs :: Expr -> Expr
simplAbs (App Abs (a :* b)) = (abs a) * (abs b)
simplAbs e                  = e


-- | Perform simplification based on properties of exponents
simplExponents :: Expr -> Expr
simplExponents (a :** N 1)    = a
simplExponents (_ :** N 0)    = num 1
simplExponents (a :** N (-1)) = recip a
simplExponents e@(a :** N n)
  | n < 0     = recip (a ** (num $ abs n))
  | otherwise = e
simplExponents e@((a :** m) :* (a' :** n))
  | a == a'   = a ** (m + n)
  | otherwise = e
simplExponents e@((a :** m) :/ (a' :** n))
  | a == a'   = a ** (m - n)
  | otherwise = e
simplExponents ((a :** m) :** n) = a ** (m * n)
simplExponents ((a :* y) :** n)  = (a ** n) * (y ** n)
simplExponents ((a :/ y) :** n)  = (a ** n) / (y ** n)
simplExponents e                 = e


-- | Perform simplification based on properties of logarithms
simplLogarithms :: Expr -> Expr
simplLogarithms (App Log ((C E) :** x)) = x
simplLogarithms (App Log (C E))         = num 1
simplLogarithms (App Log (x :* y))      = (log x) + (log y)
simplLogarithms (App Log (x :** y))     = y * (log x)
simplLogarithms (App Log (N 1 :/ x))    = -(log x)
simplLogarithms (App Log (N 1))         = num 0
simplLogarithms (App Exp (App Log x))   = x
simplLogarithms e                       = e


-- | Full simplifier
simplifier :: Expr -> Expr
simplifier e = g e
  where
    fs = [simplEval
         ,simplCancellation
         ,simplArithmetic
         ,simplAbs
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

