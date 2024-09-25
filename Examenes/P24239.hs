r2i :: Char -> Int 
r2i sim 
    | sim == 'I' = 1
    | sim == 'V' = 5
    | sim == 'X' = 10
    | sim == 'L' = 50
    | sim == 'C' = 100
    | sim == 'D' = 500 
    | sim == 'M' = 1000
    | otherwise = 0

roman2int :: String -> Int
roman2int [x] = r2i x
roman2int (x:x2:xs) 
    | r2i x < r2i x2 = - r2i x + roman2int (x2:xs) 
    | otherwise = r2i x + roman2int (x2:xs)


roman2int' :: String -> Int
roman2int' num 
    = sum [y*r2i (num2 !! x) | x <- [0..(length num-1)], let y = f (r2i (num2 !! x)) (r2i (num2 !! (x+1)))]
    where 
        num2 = num ++ [' ']
        f x y 
            | x < y = -1 
            | otherwise = 1


arrels :: Float -> [Float]
arrels num = scanl (\acc x -> 0.5*(acc + num/acc)) num [1..]


arrelAux :: [Float] -> Float -> Float
arrelAux (x:x2:xs) error 
    | abs (x - x2) <= error = x2 
    | otherwise = arrelAux (x2:xs) error


arrel :: Float -> Float -> Float
arrel num
    = arrelAux (arrels num)


data LTree a = Leaf a | Node (LTree a) (LTree a)
t1 = Node (Leaf "a") (Node (Leaf "b") (Leaf "c"))
t2 = Node (Leaf 0) (Node (Leaf 1) (Leaf 2))
t3 = Node (Node (Leaf 1) (Leaf 2)) (Leaf 0)

instance Show a => Show (LTree a) where
    show (Leaf v) = "{" ++ (show v) ++ "}"
    show (Node fe fd) = "<" ++ (show fe) ++ "," ++ (show fd) ++ ">"


build :: [a] -> LTree a
build [x] = Leaf x
build l = Node (build l1) (build l2)
    where
        len = (div ((length l) + 1) 2)
        l1 = take len l
        l2 = drop len l


zipLTrees :: LTree a -> LTree b -> Maybe (LTree (a,b))
zipLTrees (Leaf v) (Node fe fd) = Nothing
zipLTrees (Node fe fd) (Leaf v) = Nothing 
zipLTrees (Leaf v1) (Leaf v2) = Just (Leaf (v1, v2))
zipLTrees (Node fe1 fd1) (Node fe2 fd2) = do 
    v1 <- zipLTrees fe1 fe2 
    v2 <- zipLTrees fd1 fd2 
    return (Node v1 v2)