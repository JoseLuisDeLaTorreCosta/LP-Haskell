import Data.List


myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr = unfoldr


myReplicate :: a -> Int -> [a]
myReplicate val
    = myUnfoldr (\x -> if x == 0 then Nothing else Just (val, x-1))


myIterate :: (a -> a) -> a -> [a]
myIterate f
    = myUnfoldr (\x -> Just (x, f x))


myMap :: (a -> b) -> [a] -> [b]
myMap f
    = myUnfoldr (\l -> if null l then Nothing else Just (f (head l), tail l))


data Bst a = Empty | Node a (Bst a) (Bst a)

add :: Ord a => a -> (Bst a) -> (Bst a)
add x Empty = Node x Empty Empty
add x (Node y l r)
    | x < y          = Node y (add x l) r
    | x > y          = Node y l (add x r)
    | otherwise = Node y l r

instance Show a => Show (Bst a) where
    show Empty = "."
    show (Node v fe fd) = "(" ++ show v ++ " " ++ show fe ++ " " ++ show fd ++ ")"


adder :: Ord a => (Bst a, [a]) -> Maybe (Bst a, (Bst a, [a]))
adder (tree, list)
    | null list = Nothing
    | otherwise = Just (add (head list) tree, (add (head list) tree, tail list))