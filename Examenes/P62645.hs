suma :: [String] -> Int
suma [] = 0
suma (x:xs) = (read x :: Int) + suma xs


main :: IO()
main = do 
    input <- getContents 
    let x = suma (words input) 
    putStrLn (show x)
