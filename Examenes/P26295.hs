import Data.List (sort)


counting :: [String] -> [(String, Int)]
counting
    = foldr (\x acc -> f x acc) []
        where 
            f el [] = [(el, 1)]
            f el ((a, b):ls) 
                | a == el = (a, b+1):ls 
                | otherwise = (el, 1):(a, b):ls


main :: IO ()
main = do 
    input <- getContents 
    let sol = counting $ sort $ words input
    mapM_ (\(x, y) -> putStrLn (x ++ " " ++ show y)) sol