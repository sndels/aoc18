module Day9 where

import qualified Data.List as List
import qualified Data.Sequence as Seq
import Control.Exception
import Debug.Trace

parseInput :: String -> (Int, Int)
parseInput input = do
    let splitInput = words input
    (read (splitInput !! 0), read (splitInput !! 6))

data Board = Board{ current :: Int,
                    marbles :: Seq.Seq Int }
instance Show Board where
    show (Board current marbles) = do
        let currentMarble = Seq.index marbles current
        foldl (\str marble -> if marble == currentMarble 
                                        then str ++ "(" ++ (show marble) ++ ") "
                                        else str ++ (show marble) ++ " ") "" marbles

data State = State{ player :: Int,
                    marble :: Int,
                    scores :: Seq.Seq Int,
                    board :: Board }
instance Show State where
    show (State p marble scores board) = "[" ++ (show (p + 1)) ++ "]" ++ " " ++ (show scores)

-- | Inserts given marble in board at current + 2
insertMarble :: Board -> Int ->  Board
insertMarble (Board current marbles) marble = do
    let i = (mod (current + 1) $ length marbles) + 1
    (Board i (Seq.insertAt i marble marbles))

-- | Removes current - 7 from board and sets the following marble as current
removeMarble :: Board -> (Board, Int)
removeMarble (Board current marbles) = do
    let relI = mod (current - 7) $ length marbles
    let marble  = Seq.index marbles relI
    let newMarbles = Seq.deleteAt relI marbles
    let nextI = mod relI $ length newMarbles
    ((Board nextI newMarbles), marble)

-- | Plays until marble removal with given value is encountered
play :: State -> Int -> Seq.Seq Int
play (State p marble scores board) lastWorth = do
    let nextP = mod (p + 1) $ length scores
    if mod marble 23 == 0
        then do
            let (newBoard, dropped) = removeMarble board
            let value = dropped + marble
            let newScores = Seq.adjust (\score -> score + value) p scores
            if value == lastWorth || marble > lastWorth
                then trace ("Last marble worth " ++ (show value)) newScores
                else play (State nextP (marble + 1) newScores newBoard) lastWorth
        else play (State nextP (marble + 1) scores (insertMarble board marble)) lastWorth

d9p1 :: String -> IO ()
d9p1 input = do
    let testInput = "9 players; last marble is worth 32 points"
    let (players, lastWorth) = parseInput testInput
    let zeroScores = Seq.fromList . take players $ repeat 0
    let scores = play (State 0 1 zeroScores (Board 0 (Seq.fromList [0]))) lastWorth
    let highScore = (\v -> assert (True) v) $ maximum scores
    putStrLn ("High score is " ++ (show highScore))

d9p2 :: String -> IO ()
d9p2 input = do
    putStrLn "unimplemented"
