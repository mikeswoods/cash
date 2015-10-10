{-# OPTIONS -Wall -fhelpful-errors -fno-warn-unused-binds #-}

--------------------------------------------------------------------------------

module Tests.Engine.Expression.Base
    (
     (~~)
    ,defaultSymbols
    ,assignSymbols
    ,assignSymbolsSubset
    ,assignRandomSymbols
    ,assignRandomSymbolsSubset
    ,evalAndCompare
    ,evalAndCompare'
    ,evalWithRandomSymbols
    ,runTestSuite
    )
where

import Test.QuickCheck
import Control.Monad
import Engine.Expression.Core
import Engine.Expression.Common
import Engine.Rules.Simplify

--------------------------------------------------------------------------------

-- |
evalWithRandomSymbols :: (RealFloat a, Arbitrary a, Trigonometric a) => Expr a -> IO Bool
evalWithRandomSymbols e = do
    env <- assignRandomSymbols $ symbols e
    let f  = evaluate env e
    let f' = evaluate env $ simplify e
    return $ f ~~ f'


-- |
evalAndCompare :: (RealFloat a, Trigonometric a) => a -> Expr a -> (Maybe (Expr a), Maybe (Expr a))
evalAndCompare initValue e = (f, f')
  where
    env = assignSymbols (pure initValue) $ symbols e
    f   = evaluate env e
    f'  = evaluate env $ simplify e


-- |
evalAndCompare' :: (RealFloat a, Trigonometric a) => a -> Expr a -> Bool
evalAndCompare' initValue e = f ~~ f'
    where
        (f,f') = evalAndCompare initValue e


-- |
runTestSuite :: [(String, Expr Double -> Property)] -> IO ()
runTestSuite tests = do
    forM_ tests $ \(name,prop) -> do
        putStrLn name
        quickCheckWith stdArgs { maxSuccess = 500 } prop
        putStrLn ""
