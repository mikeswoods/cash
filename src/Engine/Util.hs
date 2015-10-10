{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------

module Engine.Util
    (
     (=~)
    ,epsilon
    ,euler
    ,toWholeNumber
    ,isWholeNumber
    ,prettyNumber
    ,genVarName
    ,chooseK
    ,chooseSubset
    )
where

import Control.Monad
import System.Random
import Test.QuickCheck

--------------------------------------------------------------------------------

-- | Epsilon value
epsilon :: (Num a, Fractional a, Floating a, RealFrac a, RealFloat a) => a
epsilon = 1.0e-10


-- | Euler's constant
euler :: (Num a, Fractional a, Floating a, RealFrac a, RealFloat a) => a
euler = exp 1.0


-- | Floating point equality test
(=~) :: (Eq a, RealFloat a) => a -> a -> Bool
x =~ y
    | isNaN x && isNaN y           = True
    | isInfinite x && isInfinite y = True
    | x == y                       = True
    | otherwise                    = cond1 || cond2
        where
            sx    = significand x
            sy    = significand y
            ex    = exponent x
            ey    = exponent y
            cond1 = (abs $ x - y) < epsilon
            cond2 = (ex == ey) && ((abs $ sx - sy) < epsilon)


-- | Converts a real valued number to an integral value
--
-- >>> toWholeNumber 3.1415
-- 3
toWholeNumber :: (Num a, Fractional a, Floating a, RealFrac a, Integral b) => a -> b
toWholeNumber = truncate


-- | Tests if a real valued number is a whole number
--
-- >>> toWholeNumber 3.1415
-- False
--
-- >>> toWholeNumber 3.0
-- True
--
-- >>> isWholeNumber 3
-- True
isWholeNumber :: (Num a, Fractional a, Floating a, RealFrac a) => a -> Bool
isWholeNumber x = x == (fromInteger $ round x)


-- | 'Pretty' prints a number
--
-- prettyNumber 3.1415
-- >>> "3.1415"
--
-- prettyNumber 3.0
-- >> "3"
--
-- prettyNumber 3
-- >> "3"
prettyNumber :: (Num a, Fractional a, Floating a, RealFrac a, Show a) => a -> String
prettyNumber n
  | isWholeNumber n = show $ floor n
  | otherwise       = show n


-- | QuickCheck generator the generates up to 2 letter variable names
--
-- >>> sample genVarNames
-- "v_pf0"
-- "v_tE8"
-- "v_yu3"
-- "v_i1"
-- "v_Fi0"
-- "v_P6"
-- "v_ep8"
-- "v_BA0"
-- "v_P8"
-- "v_i5"
-- "v_B5"
genVarName :: Gen String
genVarName = do
    n <- resize 2 $ listOf1 $ elements $ ['a'..'z'] ++ ['A' .. 'Z']
    d <- elements ['0' .. '9']
    return $ "v_" ++ n ++ [d]


-- | Given a number k and a list of items, min(k, |items|) elements will
-- be returned
--
-- >>> chooseK 5 [-1000 .. 1000]
-- [-451,649,238,-136,-503]
chooseK :: Int -> [a] -> IO [a]
chooseK k items = do
    forM [1 .. (min k n)] $ \_ -> do
        g <- newStdGen
        let (i, _) = randomR (0, n - 1) g
        return $ items !! i
    where
        n = length items


-- Picks a subset of elements from the list of items
--
-- >>> chooseSubset [1..10]
-- [8,10,3,10,4,1,7,8,3]
chooseSubset :: [a] -> IO [a]
chooseSubset items = do
    g <- newStdGen
    let k = fst $ randomR (0, length items) g
    chooseK k items

