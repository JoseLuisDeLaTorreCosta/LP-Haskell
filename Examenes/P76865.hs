data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)


instance Foldable Tree where 
    foldr f ini Empty = ini 
    foldr f ini (Node v fe fd) = f v (foldr f (foldr f ini fd) fe)


avg :: Tree Int -> Double
avg t = fromIntegral (sum t)/fromIntegral (length t)


cat :: Tree String -> String
cat t = tail $ foldl (\acc x -> acc ++ " " ++ x) "" t