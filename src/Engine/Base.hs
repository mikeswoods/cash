{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}

module Engine.Base
    (symbol
    ,simplify
    ,diff
    )
where


import Engine.Expression (symbol)
import Engine.Rules.Simplify (simplify)
import Engine.Rules.Diff (diff)
