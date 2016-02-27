    {-# OPTIONS -Wall -fhelpful-errors -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------

module Engine.Rules.Diff
where

import Engine.Expression.Core
import Engine.Expression.Common
  (
   sq
  ,num
  ,symbol
  ,symbols
  ,sym'
  )
import Engine.Util
  (
   euler
  )
import Engine.Rules.Simplify
  (
   simplify
  )

--------------------------------------------------------------------------------

-- | A type encoding a function f and its derivative f', e.g. <f,f'>
newtype Deriv a = D (Expr a, Expr a) deriving (Eq, Show)


-- instance (Show a, Floating a, RealFrac a) => Show (Deriv a) where
--     show (D (f, f')) = printf "D { %s : %s }" (show f) (show f')

-- | Undefined value
dxUndefined :: Deriv a
dxUndefined = D (Undefined, Undefined)

-- | Constant rule :: d/dx(c) = 0
dxConst :: Num a => a -> Deriv a
dxConst n = D (num n, num 0)


-- | Symbol rule :: d/dx(x) = 1
dxSym :: Num a => Symbol -> Deriv a
dxSym s = D (sym' s, num 1)


-- | Symbol rule for partial derivatives
dxSymPartial :: Num a => Symbol -> Deriv a
dxSymPartial s = D (sym' s, num 0)


-- | Sum rule (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxSum :: (Eq a, Num a) =>Deriv a -> Deriv a -> Deriv a
dxSum (D (f, f')) (D (g, g')) = D (f + g, f' + g')


-- | Difference rule  (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxDifference :: (Eq a, Num a) =>Deriv a -> Deriv a -> Deriv a
dxDifference (D (f, f')) (D (g, g')) = D (f - g, f' - g')


-- | Product rule (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxProduct :: (Eq a, Num a) => Deriv a -> Deriv a -> Deriv a
dxProduct (D (f, f')) (D (g, g')) = D (f * g, (f * g') + (f' * g))


-- | Quotient rule (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxQuotient :: (Eq a, Fractional a) => Deriv a -> Deriv a -> Deriv a
dxQuotient (D (f, f')) (D (g, g')) = D (f / g, ((f' * g) + (f * g')) / (sq g))


-- | Reciprocal rule
dxRecip :: (Eq a, Fractional a) => Deriv a -> Deriv a
dxRecip (D (f, f')) = D (recip f, (-(f')) / sq f)


-- | Power rule (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxPower :: (Eq a, Floating a) => Deriv a -> Deriv a -> Deriv a
dxPower (D (f, f')) (D (k, _)) = D (f ** k, (k * (f ** (k - num 1))) * f')


-- | abs (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxAbs :: (Eq a, Num a) => Deriv a -> Deriv a
dxAbs (D (f, f')) = D (abs f, f' * signum f)


-- | exp (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxExp :: (Eq a, Floating a) => Deriv a -> Deriv a
dxExp (D (f, f')) = D (exp f, f' * exp f)


-- | log (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxLog :: (Eq a, Floating a) => Deriv a -> Deriv a
dxLog (D (f, f'))  = D (log f, f' / f)


-- | ln
dxLn :: (Eq a, Floating a) => Deriv a -> Deriv a
dxLn = dxLog


-- | sqrt
dxSqrt :: (Eq a, Floating a) => Deriv a -> Deriv a
dxSqrt (D (f, f')) = D (sqrt f, f' / (2.0 * sqrt f))


-- | sin (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxSin :: (Eq a, Floating a) => Deriv a -> Deriv a
dxSin (D (f, f')) = D (sin f, (cos f) * f')


-- | cos (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxCos :: (Eq a, Floating a) => Deriv a -> Deriv a
dxCos (D (f, f')) = D (cos f, -(sin f) * f')


-- | tan (http://www.math.com/tables/derivatives/tableof.htm)
dxTan :: (Eq a, Floating a) => Deriv a -> Deriv a
dxTan (D (f, f')) = D (sin f / cos f, (sq $ sec f) * f')


-- | csc (http://www.math.com/tables/derivatives/tableof.htm)
dxCsc :: (Eq a, Floating a) => Deriv a -> Deriv a
dxCsc (D (f, f')) = D (recip $ sin f, (-(csc f) * cot f) * f')


-- | sec (http://www.math.com/tables/derivatives/tableof.htm)
dxSec :: (Eq a, Floating a) => Deriv a -> Deriv a
dxSec (D (f, f')) = D (recip $ cos f, (sec f * tan f) * f')


-- | cot (http://www.math.com/tables/derivatives/tableof.htm)
dxCot :: (Eq a, Floating a) => Deriv a -> Deriv a
dxCot (D (f, f')) = D (recip $ tan f, (-(sq $ csc f)) * f')


-- | asin (http://www.math.com/tables/derivatives/tableof.htm)
dxASin :: (Eq a, Floating a) => Deriv a -> Deriv a
dxASin (D (f, f')) = D (asin f, (num 1 / (sqrt $ 1 - sq f)) * f')


-- | acos (http://www.math.com/tables/derivatives/tableof.htm)
dxACos :: (Eq a, Floating a) => Deriv a -> Deriv a
dxACos (D (f, f')) = D (acos f, -(num 1 / (sqrt $ 1 - sq f)) * f')


-- | atan (http://www.math.com/tables/derivatives/tableof.htm)
dxATan :: (Eq a, Floating a) => Deriv a -> Deriv a
dxATan (D (f, f')) = D (atan f, (num 1 / sq f) * f')


-- | asinh (http://www.math.com/tables/derivatives/tableof.htm)
dxASinH :: (Eq a, Floating a) => Deriv a -> Deriv a
dxASinH (D (f, f')) = D (asinh f, num 1 / (sq f + num 1) * f')


-- | acosh (http://www.math.com/tables/derivatives/tableof.htm)
dxACosH :: (Eq a, Floating a) => Deriv a -> Deriv a
dxACosH (D (f, f')) = D (acosh f, (num 1 / (sq f - num 1)) * f')


-- | atanh (http://www.math.com/tables/derivatives/tableof.htm)
dxATanH :: (Eq a, Floating a) => Deriv a -> Deriv a
dxATanH (D (f, f')) = D (atanh f, (num 1 / (num 1 - sq f)) * f')


instance (Eq a, Num a) => Num (Deriv a) where
    fromInteger n      = D (num $ fromInteger n, num 0)
    (+)                = dxSum
    (-)                = dxDifference
    (*)                = dxProduct
    negate (D (f, f')) = D (negate f, negate f')
    abs                = dxAbs
    signum (D (f, _))  = D (signum f, num 0)


instance (Eq a, Fractional a) => Fractional (Deriv a) where
    fromRational n = D (num $ fromRational $ n, num 0)
    1 / y          = dxRecip y
    x / y          = dxQuotient x y
    recip          = dxRecip


-- Formulas from
-- * http://www.math.com/tables/derivatives/tableof.htm
-- * http://www.analyzemath.com/calculus/Differentiation/table_of_derivatives.html
instance (Eq a, Floating a) => Floating (Deriv a) where
    pi    = dxConst pi
    exp   = dxExp
    log   = dxLog
    (**)  = dxPower
    sqrt  = dxSqrt
    sin   = dxSin
    cos   = dxCos
    tan   = dxTan
    asin  = dxASin
    acos  = dxACos
    atan  = dxATan
    sinh  = cosh
    cosh  = sinh
    asinh = dxASinH
    acosh = dxACosH
    atanh = dxATanH


ln :: (Eq a, Floating a) => Deriv a -> Deriv a
ln = log


instance (Eq a, Floating a, Trigonometric a) => Trigonometric (Deriv a) where
  csc = dxCsc
  sec = dxSec
  cot = dxCot


-- |
isConst :: Expr a -> Bool
isConst (N _)     = True
isConst (C _)     = True
isConst (S _)     = False
isConst (f :+ g)  = (isConst f) && (isConst g)
isConst (f :- g)  = (isConst f) && (isConst g)
isConst (f :* g)  = (isConst f) && (isConst g)
isConst (f :/ g)  = (isConst f) && (isConst g)
isConst (f :** g) = (isConst f) && (isConst g)
isConst (App _ f) = isConst f
isConst Undefined = False


-- | lifts an expression (Expr) into a derivative (Deriv) with respect to a
-- | given symbol
liftD :: (Eq a, Num a, Floating a, Trigonometric a, RealFloat a)
  => Symbol
  -> Expr a
  -> Deriv a
liftD _ (N n)           = dxConst n
liftD _ (C Pi)          = dxConst $ pi
liftD _ (C E)           = dxConst $ euler
liftD s' (S s)
    | s == s'   = dxSym s
    | otherwise = dxSymPartial s
liftD wrt (f :+ g)      = (liftD wrt f) + (liftD wrt g)
liftD wrt (f :- g)      = (liftD wrt f) - (liftD wrt g)
liftD wrt (f :* g)      = (liftD wrt f) * (liftD wrt g)
liftD wrt (f :/ g)      = (liftD wrt f) / (liftD wrt g)
liftD wrt (f :** g)     = (liftD wrt f) ** (liftD wrt g)
liftD wrt (App Abs f)   = sin $ liftD wrt f
liftD wrt (App Neg f)   = -(liftD wrt f)
liftD wrt (App Log f)   = log $ liftD wrt f
liftD wrt (App Sec f)   = sec $ liftD wrt f
liftD wrt (App Csc f)   = csc $ liftD wrt f
liftD wrt (App Cot f)   = cot $ liftD wrt f
liftD wrt (App Exp f)   = exp $ liftD wrt f
liftD wrt (App Sqrt f)  = sqrt $ liftD wrt f
liftD wrt (App Sin f)   = sin $ liftD wrt f
liftD wrt (App Cos f)   = cos $ liftD wrt f
liftD wrt (App Tan f)   = tan $ liftD wrt f
liftD wrt (App ASin f)  = asin $ liftD wrt f
liftD wrt (App ACos f)  = acos $ liftD wrt f
liftD wrt (App ATan f)  = atan $ liftD wrt f
liftD wrt (App SinH f)  = sinh $ liftD wrt f
liftD wrt (App CosH f)  = cosh $ liftD wrt f
liftD wrt (App TanH f)  = tanh $ liftD wrt f
liftD wrt (App ASinH f) = asinh $ liftD wrt f
liftD wrt (App ACosH f) = acosh $ liftD wrt f
liftD wrt (App ATanH f) = atanh $ liftD wrt f
liftD _ Undefined       = dxUndefined


-- | Differentiate with respect to the first symbol found in the expression
diffF :: (Eq a, Num a, RealFloat a, Floating a, Trigonometric a)
  => Expr a
  -> Expr a
diffF e = simplify $ diff firstSym e
    where
        firstSym = case symbols e of
                     (s:_) -> s
                     []    -> (symbol "x")


-- | Differentiate with respect to a given symbol
diff :: (Eq a, Num a, RealFloat a, Floating a, Trigonometric a)
  => Symbol
  -> Expr a
  -> Expr a
diff wrt e = d
    where
        D (_, d) = liftD wrt e
