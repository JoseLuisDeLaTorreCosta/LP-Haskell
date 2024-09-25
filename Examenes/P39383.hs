fact :: [Float]
fact = scanl (*) 1 [1..]

pow :: Float -> [Float]
pow num = iterate (*num) 1


exps :: Float -> [Float]
exps num = [(p !! x)/(f !! x) | x <- [0..]]
    where 
        p = pow num
        f = fact 

exponencial :: Float -> Float -> Float
exponencial num error 
    = sum (takeWhile (>= error) (exps num))