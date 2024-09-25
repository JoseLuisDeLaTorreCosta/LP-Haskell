data Stack a = Stack [a] deriving (Show)

push :: Stack a -> a -> Stack a 
push (Stack x) elem = Stack (elem:x)

pop :: Stack a -> Stack a 
pop (Stack (x:xs)) = Stack xs 

top :: Stack a -> a 
top (Stack (x:xs)) = x 

calculator :: [String] -> Stack (Either String Int) -> Either String Int
calculator [] p = top p
calculator (x:xs) p 
    | x == "+" = do 
        vx <- top p 
        let p1 = pop p 
        vy <- top p1 
        let p2 = pop p1 
        let z = vy+vx
        calculator xs (push p2 (Right z))
    | x == "-" = do 
        vx <- top p 
        let p1 = pop p 
        vy <- top p1 
        let p2 = pop p1 
        let z = vy-vx
        if z < 0 then Left "neg" 
        else calculator xs (push p2 (Right z))
    | x == "*" = do 
        vx <- top p 
        let p1 = pop p 
        vy <- top p1 
        let p2 = pop p1 
        let z = vy*vx
        calculator xs (push p2 (Right z))
    | x == "/" = do 
        vx <- top p 
        let p1 = pop p 
        vy <- top p1 
        let p2 = pop p1 
        let z = div vy vx
        if vx == 0 then Left "div0"
        else if mod vy vx > 0 then Left "divE" 
        else calculator xs (push p2 (Right z))
    | otherwise = calculator xs (push p (Right (read x :: Int)))


main :: IO()
main = do 
    input <- getLine 
    let sol = calculator (words input) (Stack [])
    print sol
    main