main::IO()
main = do 
    c <- getLine 

    if last c == 'a' || last c == 'A' then putStrLn "Hola maca!" else putStrLn "Hola maco!"

