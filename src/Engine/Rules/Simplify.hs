    {-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

--------------------------------------------------------------------------------

module Engine.Rules.Simplify
  (
   symbol
  ,simplify
  ,simplifyAll
  ,simplifyLogarithms
  ,simplifyExponents
  ,simplifyAbs
  ,simplifyLikeTerms
  ,simplifyEval
  ,simplifyArithmetic
  ,simplifyCancel
  ,simplifySigns
  )
where

--------------------------------------------------------------------------------

import Engine.Expression.Core
import Engine.Expression.Common
  (
   num
  ,symbol
  ,iterateFixed
  ,walk
  )
import Engine.Util
  (
   isWholeNumber
  )

--------------------------------------------------------------------------------
-- http://msdl.cs.mcgill.ca/people/indrani/algebra.pdf
--

-- | Evaluate numbers under basic arithmetical operations
simplifyEval :: (Num a, Floating a, RealFrac a) => Expr a -> Expr a
simplifyEval (App Neg (N n)) = num $ -n
simplifyEval (N x :+ N y)    = num $ x + y
simplifyEval (N x :- N y)    = num $ x - y
simplifyEval (N x :* N y)    = num $ x * y
simplifyEval e@(N x :/ N y)
  | isWholeNumber (x / y) = num $ x / y
  | otherwise             = e
simplifyEval (N x :** N y)   = num $ x ** y
simplifyEval x               = x


-- | Merge and eliminate signs
simplifySigns :: (Eq a, Num a, Fractional a) => Expr a -> Expr a
simplifySigns ((App Neg x) :+ (App Neg y)) = -(x + y)
simplifySigns ((App Neg x) :* (App Neg y)) = -(x * y)
simplifySigns ((App Neg x) :/ (App Neg y)) = x / y
simplifySigns ((App Neg x) :/ y)           = -(x / y)
simplifySigns (x :/ (App Neg y))           = -(x / y)
simplifySigns (App Neg x@(N 0))            = x
simplifySigns (App Neg (App Neg x))        = x
simplifySigns (x :+ (App Neg y))           = x - y
simplifySigns (x :- (App Neg y))           = x + y
simplifySigns x                            = x


-- | Perform basic cancellation steps
simplifyCancel :: (Eq a, Num a) => Expr a -> Expr a
simplifyCancel (x :+ N 0)  = x
simplifyCancel (N 0 :+ x)  = x
simplifyCancel (x :- N 0)  = x
simplifyCancel (N 0 :- x)  = -x
simplifyCancel (x :* N 1)  = x
simplifyCancel (N 1 :* x)  = x
simplifyCancel (N 0 :/ _)  = num 0
simplifyCancel (_ :* N 0)  = num 0
simplifyCancel (N 0 :* _)  = num 0
simplifyCancel (x :** N 1) = x
simplifyCancel (_ :** N 0) = num 1
simplifyCancel x           = x


-- | Perform basic arithmetical simplifications
simplifyArithmetic :: (Eq a, Floating a) => Expr a -> Expr a
simplifyArithmetic ((App Neg a) :* b) = -(a :* b)
simplifyArithmetic (a :* (App Neg b)) = -(a :* b)
--simplifyArithmetic (a :* (b :+ c)) = (a :* b) :+ (a :* c)
-- simplifyArithmetic (a :* (b :- c)) = (a :* b) :- (a :* c)
-- simplifyArithmetic ((a :* x@(S _)) :* (b :* y@(S _)))
--   | x == y    = (a * b) * (x ** num 2)
--   | otherwise = (a * b) * (x * y)
-- simplifyArithmetic e@(((a :* b) :+ (a' :* c)) :/ d)
--   | a == a' && a' == d && d /= num 0 = b + c
--   | otherwise                        = e
-- simplifyArithmetic e@((a :* x@(S _)) :+ (b :* y@(S _)))
--   | x == y    = (a + b) * x
--   | otherwise = e
-- simplifyArithmetic e@((a :* x@(S _)) :- (b :* y@(S _)))
--   | x == y    = (a - b) * x
--   | otherwise = e
simplifyArithmetic x = x


-- | Merge like terms
simplifyLikeTerms :: (Eq a, Num a, Floating a) => Expr a -> Expr a
simplifyLikeTerms ((N x) :* ((N y) :* z)) = (N $ x * y) * z
simplifyLikeTerms e@(x :* y)
  | x == y    = x ** 2
  | otherwise = e
simplifyLikeTerms x = x


-- | Perform simplification based on properties of absolute values
simplifyAbs :: (Eq a, Num a) => Expr a -> Expr a
simplifyAbs (App Abs (x :* y)) = (abs x) * (abs y)
simplifyAbs x                  = x


-- | Perform simplification based on properties of exponents
simplifyExponents :: (Ord a, Num a, Floating a) => Expr a -> Expr a
simplifyExponents ((N 1.0) :** _)            = 1.0
simplifyExponents (_ :** (N 0.0))            = 1.0
simplifyExponents (x :** (N 1.0))            = x
simplifyExponents (x :** (App Neg a))        = recip $ x ** a
simplifyExponents (App Sqrt x)               = x ** ((N 1.0) / (N 2.0))
simplifyExponents e@((x :** a@(N _)) :* (y :** b@(N _)))
  | x == y          = x ** (a + b)
  | a == b
    && a /= (N 0.0)
    && b /= (N 0.0) = (x * y) ** a
  | otherwise       = e
simplifyExponents e@((x :** a@(N _)) :/ (y :** b@(N _)))
  | x == y    = x ** (a - b)
  | a == b
    && a /= (N 0.0)
    && b /= (N 0.0) = (x / y) ** a
  | otherwise = e
simplifyExponents ((x :** a) :** b) = x ** (a * b)
simplifyExponents x = x


-- | Perform simplification based on properties of logarithms
simplifyLogarithms :: (Eq a, Num a, Floating a) => Expr a -> Expr a
simplifyLogarithms (App Log (N 1.0))       = 0.0
simplifyLogarithms (App Log (App Exp x))   = x
simplifyLogarithms (App Log (x :* y))      = (log x) + (log y)
simplifyLogarithms (App Log ((N 1.0):/ x)) = -(log x)
simplifyLogarithms (App Log (x :/ y))      = (log x) - (log y)
simplifyLogarithms (App Log (x :** y))     = y * (log x)
simplifyLogarithms (App Log (App Sqrt x))  = (1.0 / x) * (log x)
--simplifyLogarithms (App Exp (App Log x))   = x
simplifyLogarithms x                       = x


-- | Simplification rules
simplifyAlls :: (Eq a, Ord a, Num a, Floating a, RealFrac a) => [Expr a -> Expr a]
simplifyAlls = [
                simplifyLogarithms
               ,simplifyExponents
               ,simplifyAbs
               ,simplifyLikeTerms
               ,simplifyArithmetic
               ,simplifyCancel
               ,simplifySigns
               ]


-- | One rule to rule them all
simplifyAll :: (Eq a, Ord a, Num a, Floating a, RealFrac a) => Expr a -> Expr a
simplifyAll = foldr (.) id $ simplifyAlls


-- | Finalizes the simplification by rewriting the expression as a human
-- would most likely rewrite, e.g. reorder the terms of an expression by subterm exponents,
-- convert x**(1/2) to sqrt(x), etc.
finalize :: (Eq a, Ord a, Num a, Floating a, RealFrac a) => Expr a -> Expr a
finalize (x :** ((N 1) :/ (N 2))) = sqrt x
finalize x                        = x



-- | Simplifies the expression tree
simplify :: (Eq a, Ord a, Num a, Floating a, RealFrac a, RealFloat a) => Expr a -> Expr a
simplify e = walk finalize $ run e $ apply e
  where
    apply f = walk simplifyEval $ walk (iterateFixed simplifyAll) f
    run x x'
      | x == x'   = x
      | otherwise = run x' $ apply x'

