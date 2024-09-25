import Data.List

type Pos = (Int, Int)       -- la casella inferior esquerra és (1,1)
mov = [(1, -2), (2, -1), (2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2)]

dins :: Pos -> Bool
dins (x, y) = x >= 1 && x <= 8 && y >= 1 && y <= 8



moviments :: Pos -> [Pos]
moviments (x, y) = [(x + z, y + t) | (z, t) <- mov, dins (x + z, y + t)] 


potAnar :: Pos -> Pos -> Int -> Bool 
potAnar or dest (0) = or == dest
potAnar or dest salts = any (== True) (map (\x -> potAnar x dest (salts-1)) l)
    where 
        l = moviments or

potAnar3 :: Pos -> Pos -> Bool
potAnar3 or dest = potAnar or dest 3


potAnar3' :: Pos -> Pos -> Bool 
potAnar3' or dest = dest `elem` moves 
    where 
        moves = moviments or >>= moviments >>= moviments