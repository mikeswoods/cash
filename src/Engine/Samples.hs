{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-binds #-}

--------------------------------------------------------------------------------

module Engine.Samples
  (
   e1
  ,e2
  ,e3
  )
where

--------------------------------------------------------------------------------

import Engine.Expression.Core
import Engine.Expression.Common

--------------------------------------------------------------------------------

e1 :: Expr Double
e1 = (3*x_ + 1) ** 2

e2 :: Expr Double
e2 = sqrt $ (13*x_**2 - 5*x_ + 8)

e3 :: Expr Double
e3 = (1 - 4*x_ + 7*x_**5) ** 30
