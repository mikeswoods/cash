{-# OPTIONS -Wall -fhelpful-errors -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Engine.Expression
  (Constant(..)
  ,Symbol(..)
  ,Function(..)
  ,Expr(..)
  ,Eval(..)
  ,isAssocExpr
  ,symbol
  ,symbols
  ,e'
  ,num
  ,sym
  ,sym'
  ,term
  ,ln
  ,sec
  ,csc
  ,cot
  ,square)
where


import Data.List (nub)
import Text.Printf
import Engine.Util


-- |
class Num r => Eval a c r where
  eval :: a -> r


-- |
newtype Symbol = Symbol String deriving (Eq, Ord)


-- |
instance Show Symbol where

    show (Symbol s) = s


-- |
symbol :: String -> Symbol
symbol = Symbol


-- |
data Constant = Pi
              | E
              deriving (Eq, Ord)


instance (Num r, Floating r) => (Eval Constant c r) where
  eval Pi = pi
  eval E  = exp 1.0


-- |
instance Show Constant where
    show Pi           = "pi"
    show E            = "e"


-- |
data Function = Abs
              | Neg
              | Log
              | Ln
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
              deriving (Eq)


-- |
instance Show Function where
    show Abs    = "abs"
    show Neg    = "-"
    show Log    = "log"
    show Ln     = "ln"
    show Exp    = "exp"
    show Sqrt   = "sqrt"
    show Sin    = "sin"
    show Cos    = "cos"
    show Tan    = "tan"
    show Sec    = "sec"
    show Csc    = "csc"
    show Cot    = "cot"
    show ASin   = "asin"
    show ACos   = "acos"
    show ATan   = "atan"
    show SinH   = "sinh"
    show CosH   = "cosh"
    show TanH   = "tanh"
    show ASinH  = "asinh"
    show ACosH  = "acosh"
    show ATanH  = "atanh"

-- |
infixl 4 :+
infixl 4 :-
infixl 5 :*
infixl 5 :/
infixr 6 :^

data Expr = N Float
          | C Constant
          | S Symbol
          | Expr :+ Expr
          | Expr :- Expr
          | Expr :* Expr
          | Expr :/ Expr
          | Expr :^ Expr
          | App Function Expr
          deriving (Eq, Show)


instance Num Expr where
  (+)              = (:+)
  (-)              = (:-)
  (*)              = (:*)
  negate f         = App Neg f
  abs f            = App Abs f
  fromInteger e    = N (fromIntegral e :: Float)
  signum (N f)     = N (signum f)
  signum (C c)     = N (signum $ eval c)
  signum _         = N 0

instance Fractional Expr where
  fromRational n = num $ fromRational n
  (/)            = (:/)
  recip e        = num 1 / e


instance Floating Expr where
  pi          = C Pi
  exp         = App Exp
  log         = App Log
  (**)        = (:^)
  logBase x y =log y / log x
  sqrt        = App Sqrt
  sin         = App Sin
  cos         = App Cos
  asin        = App ASin
  acos        = App ACos
  atan        = App ATan
  sinh        = App SinH
  cosh        = App CosH
  tanh        = App TanH
  asinh       = App ASinH
  acosh       = App ACosH
  atanh       = App ATanH


-- |
showInfix :: Int -> Expr -> String
showInfix _ (N v)             = prettyNumber v
showInfix _ (C c)             = show c
showInfix _ (S s)             = show s
showInfix d (e1 :+ e2)        = printf "%s + %s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1 :- e2)        = printf "%s - %s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1@(N _) :* e2@(N _))
  | d == 0    = printf "%s * %s" (showInfix (d+1) e1) (showInfix (d+1) e2)
  | otherwise = printf "(%s * %s)" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1 :* e2)        = printf "%s%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1 :/ e2)        = printf "%s / %s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1 :^ e2@(N _))  = printf "(%s)^%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1 :^ e2@(C _))  = printf "(%s)^%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1 :^ e2@(S _))  = printf "(%s)^%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1 :^ e2)        = printf "(%s)^(%s)" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (App Neg n@(N _)) = printf "-%s" (showInfix (d+1) n)
showInfix d (App fn e)        = printf "%s(%s)" (show fn) (showInfix (d+1) e)
showInfix _ (App fn _)        = printf "%s(...)" (show fn)


--instance Show Expr where
--    show = showInfix 0
--instance Show Expr where
--  show (N n)      = show n
--  show (C c)      = show c
--  show (S s)      = show s
--  show (e1 :+ e2) = printf "(%s + %s)" (show e1) (show e2)
--  show (e1 :- e2) = printf "(%s - %s)" (show e1) (show e2)
--  show (e1 :* e2) = printf "(%s * %s)" (show e1) (show e2)
--  show (e1 :/ e2) = printf "(%s / %s)" (show e1) (show e2)
--  show (e1 :^ e2) = printf "(%s ^ %s)" (show e1) (show e2)
--  show (App f e)  = printf " %s(%s)" (show f) (show e)


-- |
isAssocExpr :: Expr -> Bool
isAssocExpr (N _)     = True
isAssocExpr (C _)   = True
isAssocExpr (S _)     = True
isAssocExpr (App _ _) = True
isAssocExpr (_ :+ _)        = True
isAssocExpr (_ :* _)        = True
isAssocExpr _               = False


-- | Extract all unique symbols from an expression
symbols :: Expr -> [Symbol]
symbols = nub . collect
  where
    collect :: Expr -> [Symbol]
    collect (N _)      = []
    collect (C _)      = []
    collect (S s)      = [s]
    collect (e1 :+ e2) = (collect e1) ++ (collect e2)
    collect (e1 :- e2) = (collect e1) ++ (collect e2)
    collect (e1 :* e2) = (collect e1) ++ (collect e2)
    collect (e1 :/ e2) = (collect e1) ++ (collect e2)
    collect (e1 :^ e2) = (collect e1) ++ (collect e2)
    collect (App _ e)  = collect e


-- |
num :: Float -> Expr
num = N


-- |
sym :: String -> Expr
sym = S . Symbol


-- |
sym' :: Symbol -> Expr
sym' = S


-- |
term :: Float -> String -> Expr
term coeff var = (num coeff) :* (sym var)


-- |
e' :: Expr
e' = C E


-- |
ln :: Expr -> Expr
ln e = App Ln e


-- |
sec :: Expr -> Expr
sec e = App Sec e


-- |
csc :: Expr -> Expr
csc e = App Csc e


-- |
cot :: Expr -> Expr
cot e = App Cot e


-- |
square :: Expr -> Expr
square e = e :^ num 2
