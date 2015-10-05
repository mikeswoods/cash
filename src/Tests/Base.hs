{-# OPTIONS -Wall -fhelpful-errors -fno-warn-unused-binds #-}

module Tests.Base
    (runTestSuites
    ,testSuites
    ,samples)
where


import Test.HUnit
import Tests.Samples (samples)
import Tests.Engine.Rules.Simplify (simplifyTestSuite)


-- |
testSuites :: Test
testSuites = TestList simplifyTestSuite


-- |
runTestSuites :: IO Counts
runTestSuites = runTestTT testSuites