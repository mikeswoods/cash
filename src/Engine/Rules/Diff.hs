{-# OPTIONS -Wall -fhelpful-errors -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleContexts #-}

module Engine.Rules.Diff
where

import Text.Printf
import Engine.Expression hiding (ln)
import Engine.Rules.Simplify (simplify)


-- | A type encoding a function f and its derivative f', e.g. <f,f'>
newtype Deriv = D (Expr, Expr) deriving (Eq)


instance Show Deriv where
    show (D (f, f')) = printf "D { %s : %s }" (show f) (show f')


-- | Constant rule :: d/dx(c) = 0
dxConst :: Float -> Deriv
dxConst n = D (num n, num 0)


-- | Constant rule :: d/dx(c) = 0
dxSym :: Symbol -> Deriv
dxSym s = D (sym' s, num 1)


-- | Sum rule (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxSum :: Deriv -> Deriv -> Deriv
dxSum (D (f, f')) (D (g, g')) = D (f + g, simplify $ f' + g')


-- | Difference rule  (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxDifference :: Deriv -> Deriv -> Deriv
dxDifference (D (f, f')) (D (g, g')) = D (f - g, simplify $ f' - g')


-- | Product rule (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxProduct :: Deriv -> Deriv -> Deriv
dxProduct (D (f, f')) (D (g, g')) = D (f * g, simplify $ (f * g') + (f' * g))


-- | Quotient rule (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxQuotient :: Deriv -> Deriv -> Deriv
dxQuotient (D (f, f')) (D (g, g')) = D (f / g, simplify $ ((f' * g) + (f * g')) / (sq g))


-- | Reciprocal rule
dxRecip :: Deriv -> Deriv
dxRecip (D (f, f')) = D (recip f, simplify $ -(f' / sq f))


-- | Power rule (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxPower :: Deriv -> Deriv -> Deriv
dxPower (D (f, f')) (D (k, _)) = D (f ** k, simplify $ (k * (f ** (k - num 1))) * f')


-- | abs (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxAbs :: Deriv -> Deriv
dxAbs (D (f, f')) = D (abs f, f' * signum f)


-- | exp (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxExp :: Deriv -> Deriv
dxExp (D (f, f')) = D (exp f, simplify $ f' * exp f)


-- | log (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxLog :: Deriv -> Deriv
dxLog (D (f, f'))  = D (log f, simplify $ f' / f)


dxLn :: Deriv -> Deriv
dxLn = dxLog


-- | sqrt
dxSqrt :: Deriv -> Deriv
dxSqrt (D (f, f')) = D (sqrt f, simplify $ f' / (2.0 * sqrt f))


-- | sin (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxSin :: Deriv -> Deriv
dxSin (D (f, f')) = D (sin f, simplify $ (cos f) * f')


-- | cos (https://en.wikipedia.org/wiki/Automatic_differentiation)
dxCos :: Deriv -> Deriv
dxCos (D (f, f')) = D (cos f, simplify $ -(sin f) * f')


-- | tan (http://www.math.com/tables/derivatives/tableof.htm)
dxTan :: Deriv -> Deriv
dxTan (D (f, f')) = D (sin f / cos f, simplify $ (sq $ sec f) * f')


-- | csc (http://www.math.com/tables/derivatives/tableof.htm)
dxCsc :: Deriv -> Deriv
dxCsc (D (f, f')) = D (recip $ sin f, simplify $ (-(csc f) * cot f) * f')


-- | sec (http://www.math.com/tables/derivatives/tableof.htm)
dxSec :: Deriv -> Deriv
dxSec (D (f, f')) = D (recip $ cos f, simplify $ (sec f * tan f) * f')


-- | cot (http://www.math.com/tables/derivatives/tableof.htm)
dxCot :: Deriv -> Deriv
dxCot (D (f, f')) = D (recip $ tan f, simplify $ (-(sq $ csc f)) * f')


-- | asin (http://www.math.com/tables/derivatives/tableof.htm)
dxASin :: Deriv -> Deriv
dxASin (D (f, f')) = D (asin f, simplify $ (num 1 / (sqrt $ 1 - sq f)) * f')


-- | acos (http://www.math.com/tables/derivatives/tableof.htm)
dxACos :: Deriv -> Deriv
dxACos (D (f, f')) = D (acos f, simplify $ -(num 1 / (sqrt $ 1 - sq f)) * f')


-- | atan (http://www.math.com/tables/derivatives/tableof.htm)
dxATan :: Deriv -> Deriv
dxATan (D (f, f')) = D (atan f, simplify $ (num 1 / sq f) * f')


-- | asinh (http://www.math.com/tables/derivatives/tableof.htm)
dxASinH :: Deriv -> Deriv
dxASinH (D (f, f')) = D (asinh f, simplify $ num 1 / (sq f + num 1) * f')


-- | acosh (http://www.math.com/tables/derivatives/tableof.htm)
dxACosH :: Deriv -> Deriv
dxACosH (D (f, f')) = D (acosh f, simplify $ (num 1 / (sq f - num 1)) * f')


-- | atanh (http://www.math.com/tables/derivatives/tableof.htm)
dxATanH :: Deriv -> Deriv
dxATanH (D (f, f')) = D (atanh f, simplify $ (num 1 / (num 1 - sq f)) * f')


instance Num Deriv where
    fromInteger n      = D (num $ fromInteger n, num 0)
    (+)                = dxSum
    (-)                = dxDifference
    (*)                = dxProduct
    negate (D (f, f')) = D (negate f, negate f')
    abs                = dxAbs
    signum (D (f, _))  = D (signum f, num 0)


instance Fractional Deriv where
    fromRational n = D (num $ fromRational $ n, num 0)
    (/)            = dxQuotient
    recip          = dxRecip


-- Formulas from
-- * http://www.math.com/tables/derivatives/tableof.htm
-- * http://www.analyzemath.com/calculus/Differentiation/table_of_derivatives.html
instance Floating Deriv where
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


ln :: Deriv -> Deriv
ln = log


instance Trigonometric Deriv where
  csc = dxCsc
  sec = dxSec
  cot = dxCot


-- |
isConst :: Expr -> Bool
isConst (N _)      = True
isConst (C _)      = True
isConst (S _)      = False
isConst (f :+ g) = (isConst f) && (isConst g)
isConst (f :- g) = (isConst f) && (isConst g)
isConst (f :* g) = (isConst f) && (isConst g)
isConst (f :/ g) = (isConst f) && (isConst g)
isConst (f :** g) = (isConst f) && (isConst g)
isConst (App _ f)  = isConst f


-- | lifts an expression (Expr) into a derivative (Deriv)
liftD :: Expr -> Deriv
liftD (N n)         = dxConst n
liftD (C c)         = dxConst $ eval c
liftD (S s)         = dxSym s
liftD (f :+ g)      = (liftD f) + (liftD g)
liftD (f :- g)      = (liftD f) - (liftD g)
liftD (f :* g)      = (liftD f) * (liftD g)
liftD (f :/ g)      = (liftD f) / (liftD g)
liftD (f :** g)     = (liftD f) ** (liftD g)
liftD (App Abs f)   = sin $ liftD f
liftD (App Neg f)   = -(liftD f)
liftD (App Log f)   = log $ liftD f
liftD (App Ln f)    = ln $ liftD f
liftD (App Sec f)   = sec $ liftD f
liftD (App Csc f)   = csc $ liftD f
liftD (App Cot f)   = cot $ liftD f
liftD (App Exp f)   = exp $ liftD f
liftD (App Sqrt f)  = sqrt $ liftD f
liftD (App Sin f)   = sin $ liftD f
liftD (App Cos f)   = cos $ liftD f
liftD (App Tan f)   = tan $ liftD f
liftD (App ASin f)  = asin $ liftD f
liftD (App ACos f)  = acos $ liftD f
liftD (App ATan f)  = atan $ liftD f
liftD (App SinH f)  = sinh $ liftD f
liftD (App CosH f)  = cosh $ liftD f
liftD (App TanH f)  = tanh $ liftD f
liftD (App ASinH f) = asinh $ liftD f
liftD (App ACosH f) = acosh $ liftD f
liftD (App ATanH f) = atanh $ liftD f


-- | Differentiate with respect to the first symbol found in the expression
diffF :: Expr -> Expr
diffF e = diff firstSym e
    where
        firstSym = case symbols e of
                     (s:_) -> s
                     []    -> (symbol "x")


-- | Differentiate with respect to a given symbol
diff :: Symbol -> Expr -> Expr
diff _ e = d
    where
        D (_, d) = liftD e
