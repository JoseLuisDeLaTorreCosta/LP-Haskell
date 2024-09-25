import Control.Monad
import Control.Monad.State

suma :: Int -> State Int Int
suma x = do 
    n <- get
    let v = (+) n x
    put v
    return v
