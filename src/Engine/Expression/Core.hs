{-# OPTIONS -Wall -fhelpful-errors -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------

module Engine.Expression.Core
  (
   Constant(..)
  ,Symbol(..)
  ,Function(..)
  ,Expr(..)
  ,Eval(..)
  ,Subst(..)
  ,Trigonometric(..)
  ,Env
  ,makeExpr
  )
where

import Control.Monad
  (
   liftM
  ,liftM2
  )
import Test.QuickCheck hiding
  (
   Result
  ,collect
  )
import qualified Data.Map as Map
import Engine.Util
  (
   euler
  ,genVarName
  )

--------------------------------------------------------------------------------

-- | Trigonometric functions
class (Num a, Fractional a) => Trigonometric a where
  sec :: a -> a
  csc :: a -> a
  cot :: a -> a


instance Trigonometric Float where
  sec = recip . cos
  csc = recip . sin
  cot = recip . tan


instance Trigonometric Double where
  sec = recip . cos
  csc = recip . sin
  cot = recip . tan

--------------------------------------------------------------------------------

-- | Symbol type for variables like "x"
newtype Symbol = Symbol String
               deriving (Eq, Ord, Show, Read)


instance Arbitrary Symbol where
  arbitrary = genVarName >>= return . Symbol

--------------------------------------------------------------------------------

-- | A typeclass defining a substitution operation, where
--   f is the container type
--   a is the value to substitute
--   k is the key type used for substitution
class Functor f => Subst f a k where
  -- | The substitution operation
  subst :: Map.Map k (f a) -> f a -> f a

--------------------------------------------------------------------------------

-- | Simple environment type
type Env a = [(Symbol, Expr a)]

--------------------------------------------------------------------------------

-- | For types that can be evaluated, where
--   a is the type to be evaluated
--   b is the result of the evaluation
class Functor f => Eval f a where
  -- | The evaluation operation
  eval :: f a -> f a

--------------------------------------------------------------------------------

-- | Constant
data Constant = Pi
              | E
              deriving (Eq, Show, Read)


instance Arbitrary Constant where
  arbitrary = elements [Pi, E]


constToValue :: (Num a, Floating a, RealFloat a) => Constant -> a
constToValue Pi = pi
constToValue E  = euler

--------------------------------------------------------------------------------

-- | Functions
data Function = Abs
              | Neg
              | Log
              | Exp
              | Sqrt
              | Sin
              | Cos
              | Tan
              | Sec
              | Csc
              | Cot
              | ASin
              | ACos
              | ATan
              | SinH
              | CosH
              | TanH
              | ASinH
              | ACosH
              | ATanH
              deriving (Eq, Show, Read)


instance Arbitrary Function where
  arbitrary = elements [Abs
                       ,Neg
                       ,Log
                       ,Exp
                       ,Sqrt
                       ,Sin
                       ,Cos
                       ,Tan
                       ,Sec
                       ,Csc
                       ,Cot
                       ,ASin
                       ,ACos
                       ,ATan
                       ,SinH
                       ,CosH
                       ,TanH
                       ,ASinH
                       ,ACosH
                       ,ATanH
                       ]

--------------------------------------------------------------------------------

-- | Expression ADT
infixl 4 :+
infixl 4 :-
infixl 5 :*
infixl 5 :/
infixr 6 :**

data Expr a = N a
            | C Constant
            | S Symbol
            | (Expr a) :+ (Expr a)
            | (Expr a) :- (Expr a)
            | (Expr a) :* (Expr a)
            | (Expr a) :/ (Expr a)
            | (Expr a) :** (Expr a)
            | App Function (Expr a)
            | Undefined
            deriving (Eq, Show, Read)


shrinkBinary :: (Num a, Fractional a, Floating a, Arbitrary a) => Int -> Gen (Expr a)
shrinkBinary 0 = oneof [liftM N arbitrary
                       ,liftM C arbitrary
                       ,liftM S arbitrary
                       ]
shrinkBinary k = oneof [liftM2 (:+) (shrinkBinary k') (shrinkBinary k')
                       ,liftM2 (:-) (shrinkBinary k') (shrinkBinary k')
                       ,liftM2 (:*) (shrinkBinary k') (shrinkBinary k')
                       ,liftM2 (:/) (shrinkBinary k') (shrinkBinary k')
                       ,liftM2 (:**) (shrinkBinary k') (shrinkBinary k')
                       ,liftM2 App arbitrary (shrinkBinary k')
                       ]
  where
    k' = k `div` 2


instance (Num a, Fractional a, Floating a, Arbitrary a) => Arbitrary (Expr a) where
  arbitrary = sized shrinkBinary


instance (Num a) => Num (Expr a) where
  (+)              = (:+)
  (-)              = (:-)
  (*)              = (:*)
  negate f         = App Neg f
  abs f            = App Abs f
  fromInteger e    = N (fromIntegral e)
  signum (N f)     = N (signum f)
  signum (C Pi)    = N 1
  signum _         = N 0


instance (Fractional a) => Fractional (Expr a) where
  fromRational n = N $ fromRational n
  (/)            = (:/)
  recip e        = 1 / e


instance (Floating a) => Floating (Expr a) where
  pi          = C Pi
  exp         = App Exp
  log         = App Log
  (**)        = (:**)
  logBase x y = log y / log x
  sqrt        = App Sqrt
  sin         = App Sin
  cos         = App Cos
  tan         = App Tan
  asin        = App ASin
  acos        = App ACos
  atan        = App ATan
  sinh        = App SinH
  cosh        = App CosH
  tanh        = App TanH
  asinh       = App ASinH
  acosh       = App ACosH
  atanh       = App ATanH


instance (Num a, Fractional a) => Trigonometric (Expr a) where
  csc = App Csc
  sec = App Sec
  cot = App Cot


instance Functor Expr where
  fmap f (N n)     = N $ f n
  fmap _ (C c)     = C c
  fmap _ (S s)     = S s
  fmap f (x :+ y)  = (fmap f x) :+ (fmap f y)
  fmap f (x :- y)  = (fmap f x) :- (fmap f y)
  fmap f (x :* y)  = (fmap f x) :* (fmap f y)
  fmap f (x :/ y)  = (fmap f x) :/ (fmap f y)
  fmap f (x :** y) = (fmap f x) :** (fmap f y)
  fmap f (App g x) = App g $ fmap f x
  fmap _ Undefined = Undefined


instance Applicative Expr where
  pure = N

  (N f)    <*> (N x)     = pure $ f x
  (C c)    <*> (N _)     = C c
  (S s)    <*> (N _)     = S s
  _        <*> (C c)     = C c
  _        <*> (S s)     = S s
  f        <*> (x :+ y)  = (f <*> x) :+ (f <*> y)
  f        <*> (x :- y)  = (f <*> x) :- (f <*> y)
  f        <*> (x :* y)  = (f <*> x) :* (f <*> y)
  f        <*> (x :/ y)  = (f <*> x) :/ (f <*> y)
  f        <*> (x :** y) = (f <*> x) :** (f <*> y)
  f        <*> (App g x) = App g $ f <*> x
  (f :+ g) <*> (N x)     = (f <*> pure x) :+ (g <*> pure x)
  (f :- g) <*> (N x)     = (f <*> pure x) :- (g <*> pure x)
  (f :* g) <*> (N x)     = (f <*> pure x) :* (g <*> pure x)
  (f :/ g) <*> (N x)     = (f <*> pure x) :/ (g <*> pure x)
  (f :** g) <*> (N x)    = (f <*> pure x) :** (g <*> pure x)
  (App g f) <*> (N x)    = App g $ f <*> pure x
  Undefined <*> _        = Undefined
  _ <*> Undefined        = Undefined


instance Monad Expr where
  return = N

  -- primitives
  (N x) >>= f              = f x
  (C c) >>= _              = C c
  (S s) >>= _              = S s
  -- addition
  ((N x) :+ (N y)) >>= f   = (f x) :+ (f y)
  ((N x) :+ (C c)) >>= f   = (f x) :+ (C c)
  ((N x) :+ (S s)) >>= f   = (f x) :+ (S s)
  ((C c) :+ (N y)) >>= f   = (C c) :+ (f y)
  ((C c) :+ (C c')) >>= _  = (C c) :+ (C c')
  ((C c) :+ (S s)) >>= _   = (C c) :+ (S s)
  ((S s) :+ (N y)) >>= f   = (S s) :+ (f y)
  ((S s) :+ (C c)) >>= _   = (S s) :+ (C c)
  ((S s) :+ (S s')) >>= _  = (S s) :+ (S s')
  (x :+ y) >>= f           = (x >>= f) :+ ( y >>= f)
  -- subtraction
  ((N x) :- (N y)) >>= f   = (f x) :- (f y)
  ((N x) :- (C c)) >>= f   = (f x) :- (C c)
  ((N x) :- (S s)) >>= f   = (f x) :- (S s)
  ((C c) :- (N y)) >>= f   = (C c) :- (f y)
  ((C c) :- (C c')) >>= _  = (C c) :- (C c')
  ((C c) :- (S s)) >>= _   = (C c) :- (S s)
  ((S s) :- (N y)) >>= f   = (S s) :- (f y)
  ((S s) :- (C c)) >>= _   = (S s) :- (C c)
  ((S s) :- (S s')) >>= _  = (S s) :- (S s')
  (x :- y) >>= f           = (x >>= f) :- ( y >>= f)
  -- multiplication
  ((N x) :* (N y)) >>= f   = (f x) :* (f y)
  ((N x) :* (C c)) >>= f   = (f x) :* (C c)
  ((N x) :* (S s)) >>= f   = (f x) :* (S s)
  ((C c) :* (N y)) >>= f   = (C c) :* (f y)
  ((C c) :* (C c')) >>= _  = (C c) :* (C c')
  ((C c) :* (S s)) >>= _   = (C c) :* (S s)
  ((S s) :* (N y)) >>= f   = (S s) :* (f y)
  ((S s) :* (C c)) >>= _   = (S s) :* (C c)
  ((S s) :* (S s')) >>= _  = (S s) :* (S s')
  (x :* y) >>= f           = (x >>= f) :* ( y >>= f)
  -- division
  ((N x) :/ (N y)) >>= f   = (f x) :/ (f y)
  ((N x) :/ (C c)) >>= f   = (f x) :/ (C c)
  ((N x) :/ (S s)) >>= f   = (f x) :/ (S s)
  ((C c) :/ (N y)) >>= f   = (C c) :/ (f y)
  ((C c) :/ (C c')) >>= _  = (C c) :/ (C c')
  ((C c) :/ (S s)) >>= _   = (C c) :/ (S s)
  ((S s) :/ (N y)) >>= f   = (S s) :/ (f y)
  ((S s) :/ (C c)) >>= _   = (S s) :/ (C c)
  ((S s) :/ (S s')) >>= _  = (S s) :/ (S s')
  (x :/ y) >>= f           = (x >>= f) :/ ( y >>= f)
  -- exponentiation
  ((N x) :** (N y)) >>= f  = (f x) :** (f y)
  ((N x) :** (C c)) >>= f  = (f x) :** (C c)
  ((N x) :** (S s)) >>= f  = (f x) :** (S s)
  ((C c) :** (N y)) >>= f  = (C c) :** (f y)
  ((C c) :** (C c')) >>= _ = (C c) :** (C c')
  ((C c) :** (S s)) >>= _  = (C c) :** (S s)
  ((S s) :** (N y)) >>= f  = (S s) :** (f y)
  ((S s) :** (C c)) >>= _  = (S s) :** (C c)
  ((S s) :** (S s')) >>= _ = (S s) :** (S s')
  (x :** y) >>= f          = (x >>= f) :** ( y >>= f)
  -- function application
  (App g x) >>= f          = App g $ x >>= f
  -- Undefined
  Undefined >>= _          = Undefined


instance (Num a, Fractional a, Floating a) => Subst Expr a Symbol where
  subst _ n@(N _)             = n
  subst _ c@(C _)             = c
  subst env s@(S s') =
    case (s' `Map.lookup` env) of
      Just e' -> e'
      Nothing -> s
  subst env (x :+ y)  = (subst env x) +  (subst env y)
  subst env (x :- y)  = (subst env x) -  (subst env y)
  subst env (x :* y)  = (subst env x) *  (subst env y)
  subst env (x :/ y)  = (subst env x) /  (subst env y)
  subst env (x :** y) = (subst env x) ** (subst env y)
  subst env (App f x) = App f $ subst env x
  subst _ Undefined   = Undefined


instance (Floating a, Trigonometric a, RealFloat a) => Eval Expr a where
    eval e@(N n)
      | isNaN n || isInfinite n  = Undefined
      | otherwise                = e
    eval (C c)                   = N $ constToValue c
    eval e@(S _)                 = e
    eval (Undefined :+ _)        = Undefined
    eval (_ :+ Undefined)        = Undefined
    eval (x :+ y)                = eval x + eval y
    eval (Undefined :- _)        = Undefined
    eval (_ :- Undefined)        = Undefined
    eval (x :- y)                = eval x - eval y
    eval (Undefined :* _)        = Undefined
    eval (_ :* Undefined)        = Undefined
    eval (x :* y)                = eval x * eval y
    eval (Undefined :/ _)        = Undefined
    eval (_ :/ Undefined)        = Undefined
    eval (_ :/ (N 0))            = Undefined
    eval (x :/ y)                = eval x / eval y
    eval ((N 0) :** (App Neg _)) = Undefined
    eval (x :** y)               = eval x :** eval y
    eval (App Neg (N n))         = N $ -n
    eval (App Neg e)             = App Neg $ eval e
    eval (App Abs (N n))         = N $ abs n
    eval (App Abs e)             = App Abs $ eval e
    eval (App Log (N n))         = N $ log n
    eval (App Log e)             = App Log $ eval e
    eval (App Exp (N n))         = N $ exp n
    eval (App Exp e)             = App Exp $ eval e
    eval (App Sqrt (N n))        = N $ sqrt n
    eval (App Sqrt e)            = App Sqrt $ eval e
    eval (App Sin (N n))         = N $ sin n
    eval (App Sin e)             = App Sin $ eval e
    eval (App Cos (N n))         = N $ cos n
    eval (App Cos e)             = App Cos $ eval e
    eval (App Tan (N n))         = N $ tan n
    eval (App Tan e)             = App Tan $ eval e
    eval (App Sec (N n))         = N $ sec n
    eval (App Sec e)             = App Sec $ eval e
    eval (App Csc (N n))         = N $ csc n
    eval (App Csc e)             = App Csc $ eval e
    eval (App Cot (N n))         = N $ cot n
    eval (App Cot e)             = App Cot $ eval e
    eval (App ASin (N n))        = N $ asin n
    eval (App ASin e)            = App ASin $ eval e
    eval (App ACos (N n))        = N $ acos n
    eval (App ACos e)            = App ACos $ eval e
    eval (App ATan (N n))        = N $ atan n
    eval (App ATan e)            = App ATan $ eval e
    eval (App SinH (N n))        = N $ sinh n
    eval (App SinH e)            = App SinH $ eval e
    eval (App CosH (N n))        = N $ cosh n
    eval (App CosH e)            = App CosH $ eval e
    eval (App TanH (N n))        = N $ tanh n
    eval (App TanH e)            = App TanH $ eval e
    eval (App ASinH (N n))       = N $ asinh n
    eval (App ASinH e)           = App ASinH $ eval e
    eval (App ACosH (N n))       = N $ acosh n
    eval (App ACosH e)           = App ACosH $ eval e
    eval (App ATanH (N n))       = N $ atanh n
    eval (App ATanH e)           = App ATanH $ eval e
    eval Undefined               = Undefined

  --eval e = eval' e
  --  where
  --    isFinite x = (not $ isNaN x) && (not $ isInfinite x)
  --    liftApp1 (f,f') x = do
  --      x' <- eval' x
  --      case x' of
  --        (N a) -> let r = f a
  --                 in if isFinite r
  --                    then Just . return $ r
  --                    else Nothing
  --        _     -> Just $ App f' x'
  --    liftBinOp (op, op') x y = do
  --      x' <- eval' x
  --      y' <- eval' y
  --      case (x', y') of
  --        (N a, N b) -> let r = a `op` b
  --                      in if isFinite r
  --                         then Just . return $ r
  --                         else Nothing
  --        _          -> Just $ x `op'` y
  --    eval' (N n)
  --      | isFinite n = Just $ return n
  --      | otherwise  = Nothing
  --    eval' (C c)
  --      | isFinite c' = Just $ return c'
  --      | otherwise   = Nothing
  --      where
  --        c' = constToValue c
  --    eval' (S _)         = Just e
  --    eval' (x :+ y)      = liftBinOp ((+), (:+)) x y
  --    eval' (x :- y)      = liftBinOp ((-), (:-)) x y
  --    eval' (x :* y)      = liftBinOp ((*), (:*)) x y
  --    eval' (x :/ y)      = liftBinOp ((/), (:/)) x y
  --    eval' (x :** y)     = liftBinOp ((**), (:**)) x y
  --    eval' (App Abs x)   = liftApp1 (abs, Abs) x
  --    eval' (App Neg x)   = liftApp1 (negate, Neg) x
  --    eval' (App Log x)   = liftApp1 (log, Log) x
  --    eval' (App Exp x)   = liftApp1 (exp, Exp) x
  --    eval' (App Sqrt x)  = liftApp1 (sqrt, Sqrt) x
  --    eval' (App Sin x)   = liftApp1 (sin, Sin) x
  --    eval' (App Cos x)   = liftApp1 (cos, Cos) x
  --    eval' (App Tan x)   = liftApp1 (tan, Tan) x
  --    eval' (App Sec x)   = liftApp1 (sec, Sec) x
  --    eval' (App Csc x)   = liftApp1 (csc, Csc) x
  --    eval' (App Cot x)   = liftApp1 (cot, Cot) x
  --    eval' (App ASin x)  = liftApp1 (asin, ASin) x
  --    eval' (App ACos x)  = liftApp1 (acos, ACos) x
  --    eval' (App ATan x)  = liftApp1 (atan, ATan) x
  --    eval' (App SinH x)  = liftApp1 (sinh, SinH) x
  --    eval' (App CosH x)  = liftApp1 (cosh, CosH) x
  --    eval' (App TanH x)  = liftApp1 (tanh, TanH) x
  --    eval' (App ASinH x) = liftApp1 (asinh, ASinH) x
  --    eval' (App ACosH x) = liftApp1 (acosh, ACosH) x
  --    eval' (App ATanH x) = liftApp1 (atanh, ATanH) x
  --    eval' Undefined     = Just Undefined

--------------------------------------------------------------------------------

makeExpr :: IO (Expr Double)
makeExpr = do
  e <- generate arbitrary
  return e

--------------------------------------------------------------------------------
