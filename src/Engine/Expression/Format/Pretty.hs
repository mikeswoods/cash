{-# OPTIONS -Wall -fhelpful-errors -fno-warn-unused-binds #-}

--------------------------------------------------------------------------------

module Engine.Expression.Format.Pretty
  (
   printPretty
  ,printPretty'
  ,printTree
  ,printTree'
  )
where

import Text.Printf
  (
   printf
  )
import Text.PrettyPrint
import Engine.Expression.Core
  (
   Constant(..)
  ,Symbol(..)
  ,Function(..)
  ,Expr(..)
  )
import Engine.Expression.Common
  (
   isTerm
  )
import Engine.Util
  (
   prettyNumber
  )

--------------------------------------------------------------------------------

decodeName :: Function -> String
decodeName Abs    = "abs"
decodeName Neg    = "-"
decodeName Log    = "log"
decodeName Exp    = "exp"
decodeName Sqrt   = "√"
decodeName Sin    = "sin"
decodeName Cos    = "cos"
decodeName Tan    = "tan"
decodeName Sec    = "sec"
decodeName Csc    = "csc"
decodeName Cot    = "cot"
decodeName ASin   = "asin"
decodeName ACos   = "acos"
decodeName ATan   = "atan"
decodeName SinH   = "sinh"
decodeName CosH   = "cosh"
decodeName TanH   = "tanh"
decodeName ASinH  = "asinh"
decodeName ACosH  = "acosh"
decodeName ATanH  = "atanh"


-- | Prints the expression as a neatly formatted expression string
printPretty :: (RealFrac a, Floating a, Show a) => Expr a -> String
printPretty = pp 0
  where
    parenIf depth e
      | depth == 0 = e
      | otherwise  = printf "(%s)" e
    pp :: (RealFrac a, Floating a, Show a) => Int -> Expr a -> String
    pp _ (N n)
      | n < 0     = prettyNumber n
      | otherwise = printf "%s" $ prettyNumber n
    pp _ (C Pi)                = "π"
    pp _ (C E)                 = "e"
    pp _ (S (Symbol s))        = s
    pp d (x :+ y)              = parenIf d $ printf "%s + %s" (pp (d+1) x) (pp (d+1) y)
    pp d (x :- y)              = parenIf d $ printf "%s - %s" (pp (d+1) x) (pp (d+1) y)
    pp d e@(x :* y)
      | isTerm e               = printf "%s%s" (pp (d+1) x) (pp (d+1) y)
      | otherwise              = parenIf d $ printf "%s * %s" (pp (d+1) x) (pp (d+1) y)
    pp d (x :/ y)              = parenIf d $ printf "%s / %s" (pp (d+1) x) (pp (d+1) y)
    pp d (x@(N _) :** y@(N _)) = printf "%s**%s" (pp (d+1) x) (pp (d+1) y)
    pp d (x@(N _) :** y@(C _)) = printf "%s**%s" (pp (d+1) x) (pp (d+1) y)
    pp d (x@(N _) :** y@(S _)) = printf "%s**%s" (pp (d+1) x) (pp (d+1) y)
    pp d (x@(C _) :** y@(N _)) = printf "%s**%s" (pp (d+1) x) (pp (d+1) y)
    pp d (x@(C _) :** y@(C _)) = printf "%s**%s" (pp (d+1) x) (pp (d+1) y)
    pp d (x@(C _) :** y@(S _)) = printf "%s**%s" (pp (d+1) x) (pp (d+1) y)
    pp d (x@(S _) :** y@(N _)) = printf "%s**%s" (pp (d+1) x) (pp (d+1) y)
    pp d (x@(S _) :** y@(C _)) = printf "%s**%s" (pp (d+1) x) (pp (d+1) y)
    pp d (x@(S _) :** y@(S _)) = printf "%s**%s" (pp (d+1) x) (pp (d+1) y)
    pp d (x :** y@(N _))       = parenIf d $ printf "%s**%s" (pp (d+1) x) (pp (d+1) y)
    pp d (x :** y@(C _))       = parenIf d $ printf "%s**%s" (pp (d+1) x) (pp (d+1) y)
    pp d (x :** y@(S _))       = parenIf d $ printf "%s**%s" (pp (d+1) x) (pp (d+1) y)
    pp d (x :** y)             = parenIf d $ printf "%s**%s" (pp (d+1) x) (pp (d+1) y)
    pp _ (App Neg (N n))       = printf "-%s" $ show n
    pp _ (App Neg (C c))       = printf "-%s" $ show c
    pp d (App Neg x)           = parenIf d $ printf "(-%s)" (pp (d+1) x)
    pp d (App fn x)            = parenIf d $ printf "%s(%s)" (decodeName fn) (pp (d+1) x)
    pp _ Undefined             = "undefined"


-- | Like printPretty, but calls putStrLn on the output
printPretty' :: (Floating a, RealFrac a, Show a) => Expr a -> IO ()
printPretty' = putStrLn . printPretty

--------------------------------------------------------------------------------

-- | Pretty-print constant
printConst :: Constant -> Doc
printConst Pi = text "π"
printConst E  = text "e"


-- | Pretty-print function
printFunction :: Function -> Doc
printFunction Abs   = text "abs"
printFunction Neg   = text "neg"
printFunction Log   = text "log"
printFunction Exp   = text "exp"
printFunction Sqrt  = text "√"
printFunction Sin   = text "sin"
printFunction Cos   = text "cos"
printFunction Tan   = text "tan"
printFunction Sec   = text "sec"
printFunction Csc   = text "csc"
printFunction Cot   = text "cot"
printFunction ASin  = text "asin"
printFunction ACos  = text "acos"
printFunction ATan  = text "atan"
printFunction SinH  = text "sinh"
printFunction CosH  = text "cosh"
printFunction TanH  = text "tanh"
printFunction ASinH = text "asinh"
printFunction ACosH = text "acosh"
printFunction ATanH = text "atanh"


-- | Pretty-print symbol
printSymbol :: Symbol -> Doc
printSymbol (Symbol s) = text $ "'" ++ s ++ "'"


-- | Pretty-print expression
printExpr :: Show a => Expr a -> Doc
printExpr = printExpr' 0
    where
        nest' d = nest (d * 2)
        printExpr' d (N n)     = nest' d $ braces $ text $ show n
        printExpr' d (C c)     = nest' d . braces $ printConst c
        printExpr' d (S s)     = nest' d . braces $ printSymbol s
        printExpr' d (x :+ y)  = (nest' d $ printExpr' (d+1) x)
                              $+$
                              (nest' d $ text "<:+>")
                              $+$
                              (nest' d $ printExpr' (d+1) y)
        printExpr' d (x :- y)  = (nest' d $ printExpr' (d+1) x)
                              $+$
                              (nest' d $ text "<:->")
                              $+$
                              (nest' d $ printExpr' (d+1) y)
        printExpr' d (x :* y)  = (nest' d $ printExpr' (d+1) x)
                              $+$
                              (nest' d $ text "<:*>")
                              $+$
                              (nest' d $ printExpr' (d+1) y)
        printExpr' d (x :/ y)  = (nest' d $ printExpr' (d+1) x)
                              $+$
                              (nest' d $ text "<:/>")
                              $+$
                              (nest' d $ printExpr' (d+1) y)
        printExpr' d (x :** y) = (nest' d $ printExpr' (d+1) x)
                              $+$
                              (nest' d $ text "<:**>")
                              $+$
                              (nest' d $ printExpr' (d+1) y)
        printExpr' d (App f x) = nest' d ((text "<" <+> printFunction f <+> text ">") $+$ (printExpr' (d+1) x))
        printExpr' d Undefined = nest' d $ braces $ text "undefined"

-- | Prints the expression as a tree-link structure
printTree :: Show a => Expr a -> String
printTree e = render $ printExpr e


-- | Like printTree, but calls putStrLn on the output
printTree' :: Show a => Expr a -> IO ()
printTree' = putStrLn . printTree

