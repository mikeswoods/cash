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
  ,Trigonometric(..)
  ,isPrimitive
  ,isAssoc
  ,symbol
  ,symbols
  ,e'
  ,num
  ,sym
  ,sym'
  ,term
  ,term'
  ,sq
  ,ln
  )
where


import Data.List (nub)
import Text.Printf
import Engine.Util

--------------------------------------------------------------------------------

-- | For types that can be evaluated
class Num r => Eval a c r where
  eval :: a -> r

--------------------------------------------------------------------------------

-- | Symbol type for variables like "x"
newtype Symbol = Symbol String deriving (Eq, Ord)


instance Show Symbol where

    show (Symbol s) = s


-- | Create a new symbol from a string
symbol :: String -> Symbol
symbol = Symbol

--------------------------------------------------------------------------------

-- | Constant
data Constant = Pi
              | E
              deriving (Eq, Ord)


instance (Num r, Floating r) => (Eval Constant c r) where
  eval Pi = pi
  eval E  = exp 1.0


instance Show Constant where
    show Pi           = "pi"
    show E            = "e"

--------------------------------------------------------------------------------

-- | Functions
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


-- | Trigonometric functions
class Num a => Trigonometric a where
  sec :: a -> a
  csc :: a -> a
  cot :: a -> a


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


-- | Expression ADT
infixl 4 :+
infixl 4 :-
infixl 5 :*
infixl 5 :/
infixr 6 :**

data Expr = N Float
          | C Constant
          | S Symbol
          | Expr :+ Expr
          | Expr :- Expr
          | Expr :* Expr
          | Expr :/ Expr
          | Expr :** Expr
          | App Function Expr
          deriving (Eq)


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


instance Trigonometric Expr where
  csc = App Csc
  sec = App Sec
  cot = App Cot


-- | Show an expression in infix form
showInfix :: Int -> Expr -> String
showInfix _ (N n)                   = prettyNumber n
showInfix _ (C c)                   = show c
showInfix _ (S s)                   = show s
showInfix d (e1 :+ e2)              = printf "%s + %s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1 :- e2)              = printf "%s - %s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1@(N _) :* e2@(N _))  = printf "%s·%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1@(N _) :* e2@(S _))  = printf "%s%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1 :* e2)              = printf fmt (showInfix (d+1) e1) (showInfix (d+1) e2)
  where 
    fmt = if d == 0 then "%s * %s" else "(%s * %s)"
showInfix d (e1 :/ e2)              = printf "%s / %s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1@(N _) :** e2@(N _)) = printf "%s^%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1@(N _) :** e2@(C _)) = printf "%s^%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1@(N _) :** e2@(S _)) = printf "%s^%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1@(C _) :** e2@(N _)) = printf "%s^%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1@(C _) :** e2@(C _)) = printf "%s^%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1@(C _) :** e2@(S _)) = printf "%s^%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1@(S _) :** e2@(N _)) = printf "%s^%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1@(S _) :** e2@(C _)) = printf "%s^%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1@(S _) :** e2@(S _)) = printf "%s^%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1 :** e2@(N _))       = printf "(%s)^%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1 :** e2@(C _))       = printf "(%s)^%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1 :** e2@(S _))       = printf "(%s)^%s" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (e1 :** e2)             = printf "(%s)^(%s)" (showInfix (d+1) e1) (showInfix (d+1) e2)
showInfix d (App Neg n@(N _))       = printf "-%s" (showInfix (d+1) n)
showInfix d (App fn e)              = printf "%s(%s)" (show fn) (showInfix (d+1) e)


instance Show Expr where
   show = showInfix 0


-- | Tests if the expression is considered a primitive expression. Primitive
-- | expressions are constants, numbers, symbols, and {constant|numbers|symbol} :* {constant|symbol}
isPrimitive :: Expr -> Bool
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


-- | Tests if the expression is associative
isAssoc :: Expr -> Bool
isAssoc (N _)     = True
isAssoc (C _)     = True
isAssoc (S _)     = True
isAssoc (App _ _) = True
isAssoc (_ :+ _)  = True
isAssoc (_ :* _)  = True
isAssoc _         = False


-- | Extract all unique symbols from an expression
symbols :: Expr -> [Symbol]
symbols = nub . collect
  where
    collect :: Expr -> [Symbol]
    collect (N _)       = []
    collect (C _)       = []
    collect (S s)       = [s]
    collect (e1 :+ e2)  = (collect e1) ++ (collect e2)
    collect (e1 :- e2)  = (collect e1) ++ (collect e2)
    collect (e1 :* e2)  = (collect e1) ++ (collect e2)
    collect (e1 :/ e2)  = (collect e1) ++ (collect e2)
    collect (e1 :** e2) = (collect e1) ++ (collect e2)
    collect (App _ e)   = collect e


-- | Create a numeric expression from a float
num :: Float -> Expr
num = N


-- | Create a symbol expression from a string
sym :: String -> Expr
sym = S . Symbol


-- | Create a symbol expression from a symbol
sym' :: Symbol -> Expr
sym' = S


-- | Create a term ("3x") expression from a number and string
term :: Float -> String -> Expr
term coeff var = (num coeff) :* (sym var)


-- | Create a term with exponent ("3x^4") expression
term' :: Float -> String -> Float -> Expr
term' coeff var pow
  | pow /= 0  = num coeff :* (sym var :** num pow)
  | otherwise = num coeff :* sym var


-- | Euler's constant
e' :: Expr
e' = C E


-- | Natural logarithm
ln :: Expr -> Expr
ln e = App Ln e


-- | Square function
sq :: Expr -> Expr
sq e = e :** num 2
