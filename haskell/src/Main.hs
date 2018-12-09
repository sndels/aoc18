import System.Environment
import System.IO

import Day6
import Day7

main = do
    args <- getArgs
    let day = read $ args !! 0 :: Integer
    let part = read $ args !! 1 :: Integer
    input <- readFile("res/day" ++ show day ++ ".txt") 
    case day of
        6 -> case part of
            1 -> d6p1 input
            2 -> d6p2 input
        7 -> case part of
            1 -> d7p1 input
            2 -> d7p2 input
