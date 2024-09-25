multEq :: Int -> Int -> [Int]
multEq num1 num2 = scanl (\acc x -> acc*num1*num2) 1 [1..] 


posInList :: Int -> [Int] -> Int
posInList x [] = -1 
posInList x (y:ys) 
    | x == y = 0
    | sol == -1 = -1
    | otherwise = 1 + sol
    where 
        sol = posInList x ys

selectFirst :: [Int] -> [Int] -> [Int] -> [Int]
selectFirst l1 l2 l3 
    = [x | x <- l1, elem x l]
    where 
        l = [x | x <- l2, (posInList x l3) == -1 || (posInList x l2) < (posInList x l3)]


myIterate :: (a -> a) -> a -> [a]
myIterate f ini 
    = scanl (\acc x -> f acc) ini [1..]


type SymTab a = String -> Maybe a

empty :: SymTab a
empty = (\x -> Nothing)

get :: SymTab a -> String -> Maybe a
get table symbol = (table symbol)

set :: SymTab a -> String -> a -> SymTab a
set t k v x
    | k == x = return v
    | otherwise = get t x


data Expr a
    = Val a
    | Var String
    | Sum (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | Mul (Expr a) (Expr a)
    deriving Show

st1 = set (set empty "a" 1) "b" 2
st2 = set (set empty "a" 4) "b" 3
e1 = Mul (Val 5) (Sum (Var "a") (Var "b"))
e2 = Mul (Val 5) (Sum (Var "a") (Var "c"))
e3 = Sub (Var "a") (Var "b")

eval :: (Num a) => SymTab a -> Expr a -> Maybe a
eval table (Val a) = Just a 
eval table (Var x) = get table x
eval table (Sum a b) = do 
    vx <- eval table a
    vy <- eval table b 
    return (vx+vy)
eval table (Sub a b) = do 
    vx <- eval table a
    vy <- eval table b 
    return (vx-vy)
eval table (Mul a b) = do 
    vx <- eval table a
    vy <- eval table b 
    return (vx*vy)
