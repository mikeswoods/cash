{-# OPTIONS -Wall -fhelpful-errors -fno-warn-unused-binds #-}

module Tests.Samples
    (samples)
where 


import Engine.Expression


samples :: [Expr]
samples = [((term 3 "x") + num 1) :** num 2
          ,sqrt ((term 13 "x" :** num 2) - (term 5 "x") + num 8)
          ,(num 1 - term 4 "x" + (term 7 "x" :** num 5)) :** num 30
          ,((term 4 "x") + (sym "x" :** num (-5))) :** (num 1 :/ num 3)
          ,sin $ term 5 "x"
          ,e' :** (((term 5 "x") :** (num 2)) + term 7 "x" - num 13)
          ,num 2 :** (cot $ sym "x")
          ,num 3 * (tan . sqrt . sym $ "x")
          ,ln (num 17 - sym "x")
          ,log (num 4 + (cos . sym $ "x"))
          ,(cos $ sym "x") :** num 2
          ,(num 1 :/ num 5) * ((sec $ num 4 + (sym "x" :** num 3)) :** num 4)
          ,ln $ ((cos $ (term 3 "x") :** num 4) :** num 5)]
