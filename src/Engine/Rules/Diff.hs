{-# OPTIONS -Wall -fhelpful-errors -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleContexts #-}


module Engine.Rules.Diff
where

import Text.Printf
import Engine.Expression
import Engine.Rules.Simplify (simplify)


-- |
data Deriv = D Expr Expr
           deriving (Eq)


instance Show Deriv where

    show (D f f') = printf "D { %s : %s }" (show f) (show f')


-- | Constant rule :: d/dx(c) = 0
dxConst :: Float -> Deriv
dxConst n = D (num n) (num 0)


-- | Constant rule :: d/dx(c) = 0
dxSym :: Symbol -> Deriv
dxSym s = D (sym' s) (num 1)


-- | Sum rule :: d/dx(f + g) = d/dx(f) + d/dx(g)
dxSum :: Deriv -> Deriv -> Deriv
dxSum (D f f') (D g g') = D (f + g) (simplify $ f' + g')


-- | Difference rule :: d/dx(f - g) = d/dx(f) - d/dx(g)
dxDifference :: Deriv -> Deriv -> Deriv
dxDifference (D f f') (D g g') = D (f - g) (simplify $ f' - g')


-- | Product rule :: d/dx(f * g) = f*d/dx(g) + d/dx(f)*g
dxProduct :: Deriv -> Deriv -> Deriv
dxProduct (D f f') (D g g') = D (f * g) (simplify $ (f * g') + (f' * g))


-- | Quotient rule :: d/dx(f * g) = (d/dx(f)*g - f*d/dx(g)) / g^2
dxQuotient :: Deriv -> Deriv -> Deriv
dxQuotient (D f f') (D g g') = D (f / g) (simplify $ ((f' * g) + (f * g')) / (square g))


-- | exp
dxExp :: Deriv -> Deriv
dxExp (D f f') = D (exp f) (simplify $ f' * exp f) 

-- | log
dxLog :: Deriv -> Deriv
dxLog (D f f')  = D (log f) (simplify $ f' / f)


-- | sqrt
dxSqrt :: Deriv -> Deriv
dxSqrt (D f f') = D (sqrt f) (simplify $ f' / (2.0 * sqrt f))


-- | sin
dxSin :: Deriv -> Deriv
dxSin (D f f') = D (sin f) (simplify $ (cos f) * f')


-- | cos
dxCos :: Deriv -> Deriv
dxCos (D f f') = D (cos f) (simplify $ -(sin f) * f')


-- | asin
dxASin :: Deriv -> Deriv
dxASin (D f f') = D (asin f) (simplify $ (num 1 / (sqrt $ 1 - square f)) * f')


-- | acos
dxACos :: Deriv -> Deriv
dxACos (D f f') = D (acos f) (-(num 1 / (sqrt $ 1 - square f)) * f')


-- | atan
dxATan :: Deriv -> Deriv
dxATan (D f f') = D (atan f) ((num 1 / square f) * f')


-- | asinh
dxASinH :: Deriv -> Deriv
dxASinH (D f f') = D (asinh f) (num 1 / (square f + num 1) * f')


-- | acosh
dxACosH :: Deriv -> Deriv
dxACosH (D f f') = D (acosh f) ((num 1 / (square f - num 1)) * f')


-- | atanh
dxATanH :: Deriv -> Deriv
dxATanH (D f f') = D (atanh f) ((num 1 / (num 1 - square f)) * f')


instance Num Deriv where
    fromInteger n = D (num $ fromInteger n) (num 0)
    (+)             = dxSum
    (-)             = dxDifference
    (*)             = dxProduct
    negate (D f f') = D (negate f) (negate f')
    abs (D f f')    = D (abs f) (f' * signum f)
    signum (D f _)  = D (signum f) (num 0)


instance Fractional Deriv where
  fromRational n = D (num $ fromRational $ n) (num 0)
  (/)            = dxQuotient


-- Formulas from
-- * http://www.math.com/tables/derivatives/tableof.htm
-- * http://www.analyzemath.com/calculus/Differentiation/table_of_derivatives.html
instance Floating Deriv where
    pi    = dxConst pi
    exp   = dxExp
    log   = dxLog
    sqrt  = dxSqrt
    sin   = dxSin
    cos   = dxCos
    asin  = dxASin
    acos  = dxACos
    atan  = dxATan
    sinh  = cosh
    cosh  = sinh
    asinh = dxASinH
    acosh = dxACosH
    atanh = dxATanH


-- |
isConst :: Expr -> Bool
isConst (N _)      = True
isConst (C _)      = True
isConst (S _)      = False
isConst (e1 :+ e2) = (isConst e1) && (isConst e2)
isConst (e1 :- e2) = (isConst e1) && (isConst e2)
isConst (e1 :* e2) = (isConst e1) && (isConst e2)
isConst (e1 :/ e2) = (isConst e1) && (isConst e2)
isConst (e1 :^ e2) = (isConst e1) && (isConst e2)
isConst (App _ e)  = isConst e


-- | lifts an expression (Expr) into a derivative (Deriv)
liftD :: Expr -> Deriv
liftD (N n)         = dxConst n
liftD (C c)         = dxConst $ eval c
liftD (S s)         = dxSym s
liftD (e1 :+ e2)    = (liftD e1) + (liftD e2)
liftD (e1 :- e2)    = (liftD e1) - (liftD e2)
liftD (e1 :* e2)    = (liftD e1) * (liftD e2)
liftD (e1 :/ e2)    = (liftD e1) / (liftD e2)
liftD (e1 :^ e2)    = (liftD e1) ** (liftD e2)
liftD f@(App _ _)   = liftD f


-- |
diff :: Symbol -> Expr -> Expr
diff _ _ = undefined
