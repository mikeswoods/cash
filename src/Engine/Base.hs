{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}

module Engine.Base
    (simplify
    ,simpl
    ,diff
    ,symbol)
where


import Engine.Expression (symbol, symbols)
import Engine.Rules.Simplify (simplify, simpl)
import Engine.Rules.Diff (diff)

