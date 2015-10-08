{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
module Engine.Rules.Simplify
  (symbol
  ,simple
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
simplCancellation e          = e


-- Perform basic arithmetical simplifications
simplArithmetic :: Expr -> Expr
simplArithmetic (a :+ (App Neg b)) = a - b
simplArithmetic (a :* (b :+ c)) = (a * b) + (a * c)
simplArithmetic ((b :+ c) :* a) = (a * b) + (a * c)
simplArithmetic (a :* (b :- c)) = (a * b) - (a * c)
simplArithmetic ((b :- c) :* a) = (a * b) - (a * c)
simplArithmetic (a :* (b :* c)) = (a * b) + (a * c)
simplArithmetic ((b :* c) :* a) = (a * b) + (a * c)
simplArithmetic (a :* (b :/ c)) = (a * b) / c
simplArithmetic ((b :/ c) :* a) = (a * b) / c
simplArithmetic ((a :/ b) :+ (c :/ d))
  | b == d    = ((a * d) + (b * c)) / (b * d)
  | otherwise = (a + c) / b
simplArithmetic ((a :/ b) :/ (c :/ d)) = (a * d) / (b * c)
simplArithmetic ((a :/ b) :/ c) = a / (b * c)
simplArithmetic (a :/ (b :/ c)) = (a * c) / b
simplArithmetic ((a :/ b) :- (c :/ d))
  | b == d    = (a - c) / b
  | otherwise = ((a * d) - (b * c)) / (b * d)
simplArithmetic e@(((a :* b) :+ (a' :* c)) :/ d)
  | a == a' && a' == d && d /= num 0 = b + c
  | otherwise                        = e
simplArithmetic e@((a :* x@(S _)) :+ (b :* y@(S _)))
  | x == y    = (a + b) * x
  | otherwise = e
simplArithmetic e@((a :* x@(S _)) :- (b :* y@(S _)))
  | x == y    = (a - b) * x
  | otherwise = e
simplArithmetic ((a :* x@(S _)) :* (b :* y@(S _)))
  | x == y    = (a * b) * (x ** num 2)
  | otherwise = (a * b) * (x * y)
simplArithmetic e = e


-- Merge like terms
simplLikeTerms :: Expr -> Expr
simplLikeTerms e@(a :* b)
  | a == b    = a ** 2 
  | otherwise = e
simplLikeTerms e = e


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


-- | Apply the simplifier until we reach a fixed point for the expression
simple :: Expr -> Expr
simple e = run e $ applyRules e
  where
    rules = [simplEval
            ,simplCancellation
            ,simplArithmetic
            ,simplLikeTerms
            ,simplAbs
            ,simplExponents
            ,simplLogarithms
            ]
    applyRules = foldr (.) id $ reverse rules
    run x x' 
      | x == x'   = x
      | otherwise = run x' $ applyRules x'


-- | Recursive simplifier that walks the expression tree
simplify :: Expr -> Expr
simplify (x :+ y)    = simple $ ((simplify $ simple x) +  (simplify $ simple y))
simplify (x :- y)    = simple $ ((simplify $ simple x) -  (simplify $ simple y))
simplify (x :* y)    = simple $ ((simplify $ simple x) *  (simplify $ simple y))
simplify (x :/ y)    = simple $ ((simplify $ simple x) /  (simplify $ simple y))
simplify (x :** y)   = simple $ ((simplify $ simple x) ** (simplify $ simple y))
simplify (App Neg e) = App Neg $ simplify e
simplify e           = simple e

