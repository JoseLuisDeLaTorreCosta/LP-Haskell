data Stack a = Stack [a] deriving Show

push :: a -> Stack a -> Stack a 
push elem (Stack x) = Stack (elem:x)

pop :: Stack a -> Stack a 
pop (Stack (x:xs)) = Stack xs 

top :: Stack a -> a 
top (Stack (x:xs)) = x

eval :: [String] -> Stack Int -> Int
eval [] p = top p 
eval (x:xs) p
    | x == "+" = eval xs (push (b+a) insert)
    | x == "-" = eval xs (push (b-a) insert)
    | x == "*" = eval xs (push (b*a) insert)
    | x == "/" = eval xs (push (div b a) insert)
    | otherwise = eval xs (push elem p)
    where 
        a = top p 
        b = top $ pop p 
        insert = pop $ pop p
        elem = read x :: Int

eval1 :: String -> Int
eval1 expr = eval (words expr) (Stack [])


eval2 :: String -> Int
eval2 expr = top (foldl (\acc x -> f acc x) (Stack []) (words expr))
    where 
        f pila x 
            | x == "+" = push (b+a) insert
            | x == "-" = push (b-a) insert
            | x == "*" = push (b*a) insert
            | x == "/" = push (div b a) insert
            | otherwise = push elem pila
            where 
                a = top pila 
                b = top $ pop pila 
                insert = pop $ pop pila
                elem = read x :: Int


fsmap :: a -> [a -> a] -> a
fsmap
    = foldl (\acc f -> f acc)


divideNconquer :: (a -> Maybe b) -> (a -> (a, a)) -> (a -> (a, a) -> (b, b) -> b) -> a -> b
divideNconquer base divide conquer x  =
    case base x of
      Just y -> y
      Nothing -> conquer x (x1, x2) (y1, y2)
        where
            (x1, x2) = divide x
            y1 = divideNconquer base divide conquer x1
            y2 = divideNconquer base divide conquer x2


quickSort :: [Int] -> [Int]
quickSort = divideNconquer qsBase qsDivide qsConquer
    where
        qsBase :: [Int] -> Maybe [Int]
        qsBase []   = Just []
        qsBase [x]  = Just [x]
        qsBase _    = Nothing

        qsDivide :: [Int] -> ([Int], [Int])
        qsDivide (x:xs) = (lts, gts)
            where
                lts = filter (<=x) xs
                gts = filter (> x) xs

        qsConquer :: [Int] -> ([Int], [Int]) -> ([Int], [Int]) -> [Int]
        qsConquer (x:_) _ (ys1, ys2) = ys1 ++ x : ys2

data Racional = Racional Integer Integer 

instance Eq Racional where 
    (Racional n1 d1) == (Racional n2 d2) = n1 == n2 && d1 == d2 

instance Show Racional where
    show (Racional n1 d1) = show n1 ++ "/" ++ show d1
        where 
            m = gcd n1 d1

racional :: Integer -> Integer -> Racional
racional n1 d1 = Racional (div n1 m) (div d1 m)
    where 
        m = gcd n1 d1
    
numerador :: Racional -> Integer
numerador (Racional a b) = a

denominador :: Racional -> Integer
denominador (Racional a b) = b


data Tree a = Node a (Tree a) (Tree a)

recXnivells :: Tree a -> [a]
recXnivells t = recXnivells' [t]
    where recXnivells' ((Node x fe fd):ts) = x:recXnivells' (ts ++ [fe, fd])

rationalTree :: Racional -> Tree Racional 
rationalTree (Racional a b) = Node (Racional a b) (rationalTree (Racional a (a+b))) (rationalTree (Racional (a+b) b))

racionals :: [Racional]
racionals = recXnivells (rationalTree (Racional 1 1))