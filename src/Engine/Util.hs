{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}


module Engine.Util
    (sigDigits
    ,toWholeNumber
    ,isWholeNumber
    ,prettyNumber) 
where


-- |
sigDigits :: Float -> Float
sigDigits n = n - (fromIntegral $ floor n :: Float)


-- |
toWholeNumber :: Float -> Int
toWholeNumber = truncate


-- |
isWholeNumber :: Float -> Bool
isWholeNumber n = n == (fromIntegral $ toWholeNumber n :: Float)


-- |
prettyNumber :: Float -> String
prettyNumber n
  | isWholeNumber n = show $ floor n
  | otherwise     = show n


