{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Engine.Rules.Simplify
    (symbol
    ,squareToPower
    ,simplify
    ,simpl)
where


import Engine.Expression
import Engine.Util


-- |
squareToPower :: Expr -> Expr
squareToPower (App Sqrt e) = e :^ (num 1 :/ num 2)
squareToPower e            = e


-- | Fraction simplication
simplFrac :: Expr -> Expr
-- (n1 / d1) + (n2 / d2)
simplFrac e@((n1 :/ d1@(N d1')) :+ (n2 :/ d2@(N d2')))
  | isWholeNumber d1' && isWholeNumber d2' = ((n1 :* d2) :+ (n2 :* d1)) :/ (num scale)
  | otherwise                              = e
  where
    scale = (fromIntegral $ lcm (toWholeNumber d1') (toWholeNumber d2')) :: Float
-- (n1 / d1) - (n2 / d2)
simplFrac e@((n1 :/ d1@(N d1')) :- (n2 :/ d2@(N d2')))
  | isWholeNumber d1' && isWholeNumber d2' = ((n1 :* d2) :- (n2 :* d1)) :/ (num scale)
  | otherwise                              = e
  where
    scale = (fromIntegral $ lcm (toWholeNumber d1') (toWholeNumber d2')) :: Float
-- (n1 / d1) * (n2 / d2)
simplFrac ((n1 :/ d1) :* (n2 :/ d2)) = (n1 :* n2) :/ (d1 :* d2)
-- (n1 / d1) / (n2 / d2)
simplFrac ((n1 :/ d1) :/ (n2 :/ d2)) = (n1 :* d2) :/ (d1 :* n2)
-- otherwise
-- N + (n / d)
simplFrac (n@(N _) :+ frac@(_ :/ _)) = simpl $ (n :/ num 1) :+ frac
-- N - (n / d)
simplFrac (n@(N _) :- frac@(_ :/ _)) = simpl $ (n :/ num 1) :- frac
-- N * (n / d)
simplFrac (n@(N _) :* frac@(_ :/ _)) = simpl $ (n :/ num 1) :* frac
-- N / (n / d)
simplFrac (n@(N _) :/ frac@(_ :/ _)) = simpl $ (n :/ num 1) :/ frac
-- (n / d) + N 
simplFrac (frac@(_ :/ _) :+ n@(N _) ) = simpl $ frac :+ (n :/ num 1)
-- (n / d) - N
simplFrac (frac@(_:/ _) :- n@(N _) ) = simpl $ frac :- (n :/ num 1)
-- (n / d) * N
simplFrac (frac@(_ :/ _) :* n@(N _)) = simpl $ frac :* (n :/ num 1)
-- (n / d) / N
simplFrac (frac@(_ :/ _) :/ n@(N _)) = simpl $ frac :/ (n :/ num 1)
simplFrac (e@((N n1) :/ (N n2)))
  | sigDigits m == 0.0 = N m 
  | otherwise          = simpl e
  where
    m = n1 / n2
simplFrac e = e


-- | Core simplification rules
simpl :: Expr -> Expr
-- Prune zeros
simpl (e :+ (N 0.0))          = simpl e
simpl (e :- (N 0.0))          = simpl e
simpl ((N 0.0) :+ e)          = simpl e
simpl ((N 0.0) :- e)          = -simpl e
-- Distribute
simpl (m :* (n1 :+ n2))       = (simpl $ m :* n1) :+ (simpl $ m :* n2)
simpl (m :* (n1 :- n2))       = (simpl $ m :* n1) :- (simpl $ m :* n2)
--simpl (m :* (n1 :/ n2))       = (simpl $ m :* n1) :/ (simpl n2)
-- Prune num 1s
simpl ((N 1.0) :* e)          = simpl e
simpl (e :* (N 1.0))          = simpl e
-- Zero out
simpl ((N 0.0) :* _)          = num 0
simpl (_ :* (N 0.0))          = num 0
-- Eval 
simpl ((N n1) :+ (N n2))      = num $ n1 + n2
simpl ((N n1) :- (N n2))      = num $ n1 - n2
simpl ((N n1) :* (N n2))      = num $ n1 * n2
simpl ((N n1) :^ (N n2))      = num $ n1 ** n2
-- Power rules
simpl (e :^ (N 1.0))          = simpl e
simpl (_ :^ (N 0.0))          = num 1
simpl (App Neg (N n))   = N $ -n
simpl (e1 :+ e2)        = (simpl e1) :+ (simpl e2)
simpl (e1 :- e2)        = (simpl e1) :- (simpl e2)
simpl (e1 :* e2)        = (simpl e1) :* (simpl e2)
simpl (e1 :/ e2)        = (simpl e1) :/ (simpl e2)
simpl (e1 :^ e2)        = (simpl e1) :^ (simpl e2)
simpl e                 = e


-- |
simplify :: Expr -> Expr
simplify e = simplify' e $ simpl e
  where
    simplify' eOld eNew
      | eOld == eNew = eOld
      | otherwise    = simplify' eNew $ simpl eNew

