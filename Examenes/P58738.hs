data STree a = Nil | Node Int a (STree a) (STree a) deriving Show
div10 = flip div 10
t1 = Node 3 99 (Node 1 88 Nil Nil) (Node 1 22 Nil Nil)
t2 = Node 2 77 (Node 1 33 Nil Nil) Nil
t3 = Node 6 44 t1 t2
t4 = Node 7 55 t1 t2


talla :: STree a -> Int 
talla Nil = 0 
talla (Node t v fe fd) = t

isOk :: STree a -> Bool
isOk Nil = True 
isOk (Node t v fe fd) = isOk fe && isOk fd && 1 + talla fe + talla fd == t


nthElement :: STree a -> Int -> Maybe a
nthElement Nil _ = Nothing 
nthElement (Node t v fe fd) i
    | i == x = Just v
    | i < x = nthElement fe i
    | otherwise = nthElement fd (i-x)
    where 
        x = t - talla fd


mapToElements :: (a -> b) -> STree a -> [Int] -> [Maybe b]
mapToElements f t list 
    = map (\x -> fmap f (nthElement t x)) list 


instance Functor STree where 
    fmap f Nil = Nil 
    fmap f (Node t v fe fd) = Node t (f v) (fmap f fe) (fmap f fd)