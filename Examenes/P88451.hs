data Tree a = Empty | Node a (Tree a) (Tree a)

instance Show a => Show (Tree a) where 
    show Empty = "()"
    show (Node v fe fd) = "(" ++ show fe ++ "," ++ show v ++ "," ++ show fd ++ ")"


instance Functor Tree where 
    fmap f Empty = Empty
    fmap f (Node v fe fd) = Node (f v) (fmap f fe) (fmap f fd)

doubleT :: Num a => Tree a -> Tree a
doubleT = fmap (*2)


data Forest a = Forest [Tree a] deriving (Show)

instance Functor Forest where 
    fmap f (Forest x) = Forest (map (fmap f) x)

doubleF :: Num a => Forest a -> Forest a
doubleF = fmap (*2)