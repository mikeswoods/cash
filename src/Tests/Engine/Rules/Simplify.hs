{-# OPTIONS -Wall -fhelpful-errors -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleContexts #-}

module Tests.Engine.Rules.Simplify
    (
     runSimplifyTestSuite
    ,equivAfter
    ,equiv
    --  prop_evalEquivAfterSimplifyLogarithmsPure
    -- ,prop_evalEquivAfterSimplifyExponentsPure
    -- ,prop_evalEquivAfterSimplifyAbsolutesPure
    -- ,prop_evalEquivAfterSimplifyLikeTermsPure
    -- ,prop_evalEquivAfterSimplifyEvalPure
    -- ,prop_evalEquivAfterSimplifyArithmeticPure
    -- ,prop_evalEquivAfterSimplifyCancelPure
    -- ,prop_evalEquivAfterSimplifySignsPure
    -- ,prop_evalEquivAfterSimplifyPure
    -- ,prop_evalEquivAfterSimplifyImpure
    )
where

import Test.QuickCheck
    (
     Property
    ,Arbitrary
    ,ioProperty
    )
import Engine.Expression.Core
import Engine.Expression.Common (
     evaluate
    ,symbols
    )
import Engine.Rules.Simplify
import Tests.Engine.Expression.Base

--------------------------------------------------------------------------------

equiv :: Env Double -> (Expr Double -> Expr Double) -> Expr Double -> Bool
equiv env simplifyRule e = (e == f) || ((evaluate env e) ~~ (evaluate env f))
    where
        f = simplifyRule e


equivAfter :: (Expr Double -> Expr Double) -> Expr Double -> IO Bool
equivAfter simplifyRule e = do
    let syms = symbols e
    let constEnv = assignSymbols (pure pi) syms
    -- let zeroEnv  = assignSymbols (pure 0.0) syms
    -- randEnv <- assignRandomSymbols syms
    return $ and [
                  equiv constEnv simplifyRule e
                 -- ,equiv randEnv simplifyRule e
                 -- ,equiv zeroEnv simplifyRule e
                 ]


equivAfterProperty :: (Expr Double -> Expr Double) -> Expr Double -> Property
equivAfterProperty simplifyRule = ioProperty . (equivAfter simplifyRule)

--------------------------------------------------------------------------------

-- | Test simplifying logarithms (pure)
prop_evalEquivAfterSimplifyLogarithmsPure :: Expr Double -> Property
prop_evalEquivAfterSimplifyLogarithmsPure = equivAfterProperty simplifyLogarithms

--------------------------------------------------------------------------------

-- | Test simplifying exponents (pure)
prop_evalEquivAfterSimplifyExponentsPure :: Expr Double -> Property
prop_evalEquivAfterSimplifyExponentsPure = equivAfterProperty simplifyExponents

--------------------------------------------------------------------------------

-- | Test simplifying exponents (pure)
prop_evalEquivAfterSimplifyAbsolutesPure :: Expr Double -> Property
prop_evalEquivAfterSimplifyAbsolutesPure = equivAfterProperty simplifyAbs

--------------------------------------------------------------------------------

-- | Test simplifying like terms (pure)
prop_evalEquivAfterSimplifyLikeTermsPure :: Expr Double -> Property
prop_evalEquivAfterSimplifyLikeTermsPure = equivAfterProperty simplifyLikeTerms

--------------------------------------------------------------------------------

-- | Test simplifying basic evaluation (pure)
prop_evalEquivAfterSimplifyEvalPure :: Expr Double -> Property
prop_evalEquivAfterSimplifyEvalPure = equivAfterProperty simplifyEval

--------------------------------------------------------------------------------

-- | Test simplifying basic evaluation (pure)
prop_evalEquivAfterSimplifyArithmeticPure :: Expr Double -> Property
prop_evalEquivAfterSimplifyArithmeticPure = equivAfterProperty simplifyArithmetic

--------------------------------------------------------------------------------

prop_evalEquivAfterSimplifyCancelPure :: Expr Double -> Property
prop_evalEquivAfterSimplifyCancelPure = equivAfterProperty simplifyCancel

--------------------------------------------------------------------------------

prop_evalEquivAfterSimplifySignsPure :: Expr Double -> Property
prop_evalEquivAfterSimplifySignsPure = equivAfterProperty simplifySigns

--------------------------------------------------------------------------------

-- | Test full simplification (pure)
prop_evalEquivAfterSimplifyPure :: Expr Double -> Bool
prop_evalEquivAfterSimplifyPure e = f ~~ f'
  where
     env = assignSymbols 0.42 $ symbols e
     f   = evaluate env e
     f'  = evaluate env $ simplify e

--------------------------------------------------------------------------------

-- |
prop_evalEquivAfterSimplifyImpure :: (RealFloat a, Arbitrary a, Trigonometric a) => Expr a -> Property
prop_evalEquivAfterSimplifyImpure = \e -> ioProperty $ evalWithRandomSymbols e

--------------------------------------------------------------------------------

testSuite :: [(String, Expr Double -> Property)]
testSuite = [
             ("prop_evalEquivAfterSimplifyLogarithmsPure", prop_evalEquivAfterSimplifyLogarithmsPure)
            ,("prop_evalEquivAfterSimplifyExponentsPure", prop_evalEquivAfterSimplifyExponentsPure)
            ,("prop_evalEquivAfterSimplifyAbsolutesPure", prop_evalEquivAfterSimplifyAbsolutesPure)
            ,("prop_evalEquivAfterSimplifyLikeTermsPure", prop_evalEquivAfterSimplifyLikeTermsPure)
            ,("prop_evalEquivAfterSimplifyEvalPure", prop_evalEquivAfterSimplifyEvalPure)
            ,("prop_evalEquivAfterSimplifyArithmeticPure", prop_evalEquivAfterSimplifyArithmeticPure)
            ,("prop_evalEquivAfterSimplifyCancelPure", prop_evalEquivAfterSimplifyCancelPure)
            ,("prop_evalEquivAfterSimplifySignsPure", prop_evalEquivAfterSimplifySignsPure)
            --,("prop_evalEquivAfterSimplifyPure", prop_evalEquivAfterSimplifyPure)
            ]


runSimplifyTestSuite :: IO ()
runSimplifyTestSuite = runTestSuite testSuite

