import Data.Ratio

fact :: [Rational]
fact = scanl (*) 1 [1..]

pow :: Rational -> [Rational] 
pow num = iterate (* num) 1

termes_cosinus :: Rational -> [Rational]
termes_cosinus angle = [(p2 !! x)*(p !! (2*x))/(f !! (2*x)) | x <- [0..]]
    where 
        f = fact 
        p = pow angle
        p2 = pow (-1)


cosinus :: Rational -> Rational -> Rational
cosinus angle error = sum $ takeWhile (\x -> abs(x) >= abs(error)) $ termes_cosinus angle