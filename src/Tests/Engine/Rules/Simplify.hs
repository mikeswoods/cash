{-# OPTIONS -Wall -fhelpful-errors -fno-warn-unused-binds #-}

module Tests.Engine.Rules.Simplify
    (simplifyTestSuite)
where


import Test.HUnit
import Engine.Expression
import Engine.Rules.Simplify

--
mkT :: (Expr, Expr) -> Test
mkT (actual, expected) = (simplify $ actual, [] :: [Symbol]) ~?= (expected, [] :: [Symbol])


infixr 3 =?=
(=?=) :: a -> b -> (a, b)
(=?=) = (,)

-- 
simplifyTestSuite :: [Test]
simplifyTestSuite = [mkT $ num 1 + num 2 =?= num 3
                    ,mkT $ sym "x" :** num 0 =?= num 1
                    ,mkT $ num 2 + sym "x" =?= num 2 + sym "x"
                    ,mkT $ num 2 * sym "x" =?= num 2 * sym "x"
                    ,mkT $ num 2 * num 0 =?= num 0
                    ,mkT $ num 0 * num 2 =?= num 0
                    ,mkT $ sym "x" * num 0 =?= num 0
                    ,mkT $ num 0 * sym "x" =?= num 0
                    ,mkT $ sym "x" * num 1 =?= sym "x"
                    ,mkT $ num 1 * sym "x" =?= sym "x"
                    ,mkT $ num 2 + num 3 =?= num 5 
                    ,mkT $ num 2 * num 3 =?= num 6]
