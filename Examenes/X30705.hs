
main :: IO()
main = do 
    input <- getContents 
    let x = (words input)
    putStrLn (show x)