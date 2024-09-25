data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

t7 = Node 7 Empty Empty
t6 = Node 6 Empty Empty
t5 = Node 5 Empty Empty
t4 = Node 4 Empty Empty
t3 = Node 3 t6 t7
t2 = Node 2 t4 t5
t1 = Node 1 t2 t3
t1' = Node 1 t3 t2


size :: Tree a -> Int
size Empty = 0
size (Node v fe fd) = 1 + size fe + size fd


height :: Tree a -> Int
height Empty = 0
height (Node v fe fd) = 1 + max (height fe) (height fd)


equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True 
equal Empty (Node v fe fd) = False 
equal (Node v fe fd) Empty = False
equal (Node v1 fe1 fd1) (Node v2 fe2 fd2) 
    | v1 /= v2 = False 
    | otherwise = equal fe1 fe2 && equal fd1 fd2


isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty (Node _ _ _) = False
isomorphic (Node _ _ _) Empty = False
isomorphic Empty Empty = True
isomorphic (Node x1 a1 b1) (Node x2 a2 b2) = x1 == x2 &&
                                           ((equal a1 a2 &&
                                             equal b1 b2) ||
                                            (equal a1 b2 &&
                                             equal b1 a2))


preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node v fe fd) = [v] ++ preOrder fe ++ preOrder fd


postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node v fe fd) = postOrder fe ++ postOrder fd ++ [v]


inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node v fe fd) = inOrder fe ++ [v] ++ inOrder fd


breadthFirst :: Tree a -> [a]
breadthFirst t
    = bf [t]
    where 
        bf :: [Tree a] -> [a]
        bf [] = []
        bf (Empty:ts) = bf ts
        bf ((Node rt l r):ts) = rt : (bf (ts ++ [l, r])) 


build :: Eq a => [a] -> [a] -> Tree a
build [ ] [ ]              = Empty
build (x:preorder) inorder = Node x
                                 (build leftPreorder  leftInorder )
                                 (build rightPreorder rightInorder)
    where  
        leftInorder   = takeWhile (/= x) inorder
        leftPreorder  = take (length leftInorder) preorder
        rightPreorder = drop (length leftInorder) preorder
        rightInorder  = tail (dropWhile (/= x) inorder)


overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap f Empty Empty = Empty 
overlap f Empty (Node v fe fd) = Node v fe fd
overlap f (Node v fe fd) Empty = Node v fe fd
overlap f (Node v1 fe1 fd1) (Node v2 fe2 fd2) = Node (f v1 v2) (overlap f fe1 fe2) (overlap f fd1 fd2)