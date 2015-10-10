{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

--------------------------------------------------------------------------------

module Engine.Rules.Canonical
  (
   canonicalize
  ,toLeftHanded
  )
where

--------------------------------------------------------------------------------

import Engine.Expression.Core
import Engine.Expression.Common
  (
   walk
  )

--------------------------------------------------------------------------------
-- http://msdl.cs.mcgill.ca/people/indrani/algebra.pdf
--

-- | Reorders the expression tree such that constants, numbers, and symbols
-- | are on the left hand side of any associatve sub-expression
reorderLeft :: Expr a -> Expr a
reorderLeft e@((N _) :+ (N _))   = e
reorderLeft e@((N _) :+ (C _))   = e
reorderLeft e@((N _) :+ (S _))   = e
reorderLeft (x@(C _) :+ y@(N _)) = (y :+ x)
reorderLeft (x@(S _) :+ y@(N _)) = (y :+ x)
reorderLeft e@((S _) :+ _)       = e
reorderLeft (x :+ y@(N _))       = (y :+ x)
reorderLeft e@((N _) :* (N _))   = e
reorderLeft e@((N _) :* (C _))   = e
reorderLeft e@((N _) :* (S _))   = e
reorderLeft (x@(C _) :* y@(N _)) = (y :* x)
reorderLeft (x@(S _) :* y@(N _)) = (y :* x)
reorderLeft e@((S _) :* _)       = e
reorderLeft (x :* y@(N _))       = (y :* x)
reorderLeft (App f x)            = App f $ reorderLeft x
reorderLeft e                    = e


-- | Convert a tree such that primitive expressions are on the left-hand-side
-- | of all associative parent expressions, e.g 2(x + 4) becomes 2(4 + x)
toLeftHanded :: (Eq a, Num a, Floating a) => Expr a -> Expr a
toLeftHanded = walk reorderLeft

--------------------------------------------------------------------------------

canonicalizeNegative :: (Num a, Floating a, RealFrac a) => Expr a -> Expr a
canonicalizeNegative (App Neg e) = (N $ -1.0) * e
canonicalizeNegative e           = e

canonicalizeFraction :: (Num a, Floating a, RealFrac a) => Expr a -> Expr a
canonicalizeFraction (a :/ (b :** m)) = a * (b ** (-m))
canonicalizeFraction e                = e

--canonicalizeConstantFold :: (Num a, Floating a, RealFrac a) => Expr a -> Expr a
--canonicalizeConstantFold (a + (N 0))      = a
--canonicalizeConstantFold ((N 0) + b)      = b
--canonicalizeConstantFold (a * (N 1))      = a
--canonicalizeConstantFold ((N 1) + b)      = b
--canonicalizeConstantFold e                = e

--------------------------------------------------------------------------------

-- | Convert an expression to a canonicalized form
canonicalize :: (Num a, Floating a, RealFrac a) => Expr a -> Expr a
canonicalize  = canonicalizeFraction . canonicalizeNegative
