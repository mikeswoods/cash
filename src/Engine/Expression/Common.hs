{-# OPTIONS -Wall -fhelpful-errors -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------

module Engine.Expression.Common
  (
   x_
  ,y_
  ,z_
  ,inv
  ,sq
  ,num
  ,sym
  ,sym'
  ,symbol
  ,term
  ,term'
  ,symbols
  ,isScalar
  ,isTerm
  ,isPrimitive
  ,isAssociative
  ,isCommutative
  ,(~~)
  ,defaultSymbols
  ,assignSymbols
  ,assignSymbolsSubset
  ,assignRandomSymbols
  ,assignRandomSymbolsSubset
  ,evaluate
  ,evaluateWith
  ,evaluateWithZeros
  ,iterateFixed
  ,walk
  )
where

--------------------------------------------------------------------------------

import Test.QuickCheck hiding (collect)
import Engine.Expression.Core
import Engine.Util
  (
   chooseSubset
  )
import Data.List
  (
  nub
  )
import qualified Data.Map as Map

--------------------------------------------------------------------------------

-- | Square function
sq :: Num a => Expr a -> Expr a
sq x = x :** 2


-- Inverse function
inv :: (Num a, Floating a) => Expr a -> Expr a
inv x = x ** (N $ -1)


-- | Predefined expression variable "x"
x_ :: Expr a
x_ = sym "x"


-- | Predefined expression variable "y"
y_ :: Expr a
y_ = sym "y"


-- | Predefined expression variable "z"
z_ :: Expr a
z_ = sym "z"


-- | Create a numeric expression from a float
num :: Num a => a -> Expr a
num = N


-- | Create a symbol expression from a string
sym :: String -> Expr a
sym = S . Symbol


-- | Create a symbol expression from a symbol
sym' :: Symbol -> Expr a
sym' = S


-- | Create a new symbol from a string
symbol :: String -> Symbol
symbol = Symbol

-- | Create a term ("3x") expression from a number and string
term :: Num a => a -> String -> Expr a
term coeff var = (num coeff) :* (sym var)


-- | Create a term with exponent ("3x^4") expression
term' :: (Eq a, Num a) => a -> String -> a -> Expr a
term' coeff var pow
  | pow /= 0  = num coeff :* (sym var :** num pow)
  | otherwise = num coeff :* sym var

--------------------------------------------------------------------------------

-- | Tests if the expression is considered a scalar value
isScalar :: Expr a -> Bool
isScalar (N _) = True
isScalar (C _) = True
isScalar (S _) = True
isScalar _     = False


-- | Tests if the expression is a term of the form "<coeff> * <var> ** <power>"
isTerm :: Expr a -> Bool
isTerm ((x :* y) :** _)
  | isPrimitive x && isPrimitive y            = True
  | isPrimitive x && (not . isPrimitive $ y)  = True
  | (not . isPrimitive $ ) x && isPrimitive y = True
  | otherwise                                 = False
isTerm (x :* y)
  | isPrimitive x && isPrimitive y           = True
  | isPrimitive x && (not . isPrimitive $ y) = True
  | (not . isPrimitive $ x) && isPrimitive y = True
  | otherwise                                = False
isTerm _                                     = False

-- | Tests if the expression is considered a primitive expression. Primitive
-- | expressions are constants, numbers, symbols, and {constant|numbers|symbol} :* {constant|symbol}
isPrimitive :: Expr a -> Bool
isPrimitive (N _)             = True
isPrimitive (C _)             = True
isPrimitive (S _)             = True
isPrimitive ((N _ ) :* (S _)) = True
isPrimitive ((C _ ) :* (S _)) = True
isPrimitive ((S _ ) :* (S _)) = True
isPrimitive ((N _ ) :* (C _)) = True
isPrimitive ((C _ ) :* (C _)) = True
isPrimitive ((S _ ) :* (C _)) = True
isPrimitive _                 = False


-- | Tests if the expression is associative, e.g. (a `op` (b `op` c)) == ((a `op` b) `op` c)
isAssociative :: Expr a -> Bool
isAssociative (N _)     = True
isAssociative (C _)     = True
isAssociative (S _)     = True
isAssociative (App _ _) = True
isAssociative (_ :+ _)  = True
isAssociative (_ :* _)  = True
isAssociative _         = False


-- | Tests if the expression is commutative, e.g. (a `op` b) == (b `op` a)
isCommutative :: Expr a -> Bool
isCommutative (N _)     = True
isCommutative (C _)     = True
isCommutative (S _)     = True
isCommutative (App _ _) = True
isCommutative (_ :+ _)  = True
isCommutative (_ :* _)  = True
isCommutative _         = False

--------------------------------------------------------------------------------

-- | Extract all unique symbols from an expression
symbols :: Expr a -> [Symbol]
symbols = nub . collect
  where
    collect :: Expr a -> [Symbol]
    collect (N _)     = []
    collect (C _)     = []
    collect (S s)     = [s]
    collect (x :+ y)  = (collect x) ++ (collect y)
    collect (x :- y)  = (collect x) ++ (collect y)
    collect (x :* y)  = (collect x) ++ (collect y)
    collect (x :/ y)  = (collect x) ++ (collect y)
    collect (x :** y) = (collect x) ++ (collect y)
    collect (App _ x) = collect x
    collect Undefined = []

--------------------------------------------------------------------------------

-- | Expression equality test
(~~) :: (Eq a, RealFloat a) => Expr a -> Expr a -> Bool
(~~) = (==)


-- |
defaultSymbols :: Floating a => Env a
defaultSymbols = [(symbol "x", num pi)]


-- |
assignSymbols :: Expr a -> [Symbol] -> Env a
assignSymbols v syms = zip syms (repeat v)


-- |
assignSymbolsSubset :: Expr a -> [Symbol] -> IO (Env a)
assignSymbolsSubset v syms = do
    s' <- chooseSubset syms
    return $ zip s' (repeat v)


-- |
assignRandomSymbols :: (Num a, Floating a, Arbitrary a) => [Symbol] -> IO (Env a)
assignRandomSymbols syms = do
    v' <- generate $ infiniteListOf $ arbitrary
    return $ zip syms (fmap pure v')


-- |
assignRandomSymbolsSubset :: (Floating a, Arbitrary a) => [Symbol] -> IO (Env a)
assignRandomSymbolsSubset syms = do
    v' <- generate $ infiniteListOf $ arbitrary
    s' <- chooseSubset syms
    return $ zip s' v'

--------------------------------------------------------------------------------

-- | Recursively walks the expression tree applying f
walk :: (Eq a, Num a, Floating a) => (Expr a -> Expr a) -> Expr a -> Expr a
walk f (x :+ y)   = f $ ((walk f $ f x) +  (walk f $ f y))
walk f (x :- y)   = f $ ((walk f $ f x) -  (walk f $ f y))
walk f (x :* y)   = f $ ((walk f $ f x) *  (walk f $ f y))
walk f (x :/ y)   = f $ ((walk f $ f x) /  (walk f $ f y))
walk f (x :** y)  = f $ ((walk f $ f x) ** (walk f $ f y))
walk f (App f' x) = f $ App f' $ walk f x
walk f x          = f x


-- | Apply a function to until we reach a fixed point for the expression
iterateFixed :: (Eq a, Ord a, Num a, Floating a, RealFrac a, RealFloat a)
  => (Expr a -> Expr a)
  -> Expr a
  -> Expr a
iterateFixed f x = iterApply x $ f x
  where
    iterApply y y'
      | y == y'   = y
      | otherwise = iterApply y' $ f y'

--------------------------------------------------------------------------------

-- | Substitutes a symbol into the given expression, then  evaluating the
-- subsequent expression
--
-- >>> evaluate [(symbol "x", 2.0)] (sym "x" * 3)
-- Right $ N 6.0
--
-- >>> Left $ evaluate [(symbol "x", 2.0)] (sym "x" + sym "y")
-- Left $ N 2.0 :+ S "y"
--
-- >>> evaluate [(symbol "z", 2.0)] (sym "x" + sym "y")
-- Left $ S "x" :+ S "y"
evaluate :: (Ord k, Eval f a, Subst f a k) => [(k, f a)] -> f a -> f a
evaluate env x = eval $ subst (Map.fromList env) x


-- | Evaluates the given expression with a single substituted value for all
-- symbols
evaluateWith :: (RealFloat a, Trigonometric a) => a -> Expr a -> Expr a
evaluateWith initValue e = evaluate env e
  where
    syms = symbols e
    env  = assignSymbols (pure initValue) syms


-- | Evaluates the given expression with the substituted value of zero for all
-- symbols
evaluateWithZeros :: (RealFloat a, Trigonometric a) => Expr a -> Expr a
evaluateWithZeros = evaluateWith 0
