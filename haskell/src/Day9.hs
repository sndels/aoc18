module Day9 where

import qualified Data.Sequence as Seq
import Control.Exception

parseInput :: String -> (Int, Int)
parseInput input = do
    let splitInput = words input
    (read (splitInput !! 0), read (splitInput !! 6))

data State = State{ player :: Int,
                    marble :: Int,
                    scores :: Seq.Seq Int,
                    board :: Seq.Seq Int}
instance Show State where
    show (State p marble scores board) = "[" ++ (show (p + 1)) ++ "]" ++ " " ++ (show board)

-- | Inserts given marble in board at current + 2
insertMarble :: Seq.Seq Int -> Int -> Seq.Seq Int
insertMarble marbles marble = newMarbles where
    (l, r) = Seq.splitAt 2 marbles
    newMarbles = marble Seq.<| r Seq.>< l

-- | Removes current - 7 from board and sets the following marble as current
removeMarble :: Seq.Seq Int-> (Seq.Seq Int, Int)
removeMarble marbles = (newMarbles, marble) where
    (l, r) = Seq.splitAt (mod (-7) $ length marbles) marbles
    marble = Seq.index r 0
    newMarbles = (Seq.drop 1 r) Seq.>< l

-- | Plays until marble with given value is encountered
play :: State -> Int -> Seq.Seq Int
play (State p marble scores board) lastMarble = do
    let nextP = mod (p + 1) $ length scores
    if marble > lastMarble
        then scores
        else if mod marble 23 == 0
            then do
                let (newBoard, dropped) = removeMarble board
                let value = dropped + marble
                let newScores = Seq.adjust (\score -> score + value) p scores
                play (State nextP (marble + 1) newScores newBoard) lastMarble
            else play (State nextP (marble + 1) scores (insertMarble board marble)) lastMarble

d9p1 :: String -> IO ()
d9p1 input = do
    let (players, lastMarble) = parseInput input
    let zeroScores = Seq.fromList . take players $ repeat 0
    let scores = play (State 0 1 zeroScores (Seq.fromList [0])) lastMarble
    let highScore = maximum scores
    putStrLn ("High score is " ++ (show $ (\v -> assert (v == 404502) v) highScore))

d9p2 :: String -> IO ()
d9p2 input = do
    let (players, lastMarble) = parseInput input
    let zeroScores = Seq.fromList . take players $ repeat 0
    let scores = play (State 0 1 zeroScores (Seq.fromList [0])) (100 * lastMarble)
    let highScore = (\v -> assert (True) v) $ maximum scores
    putStrLn ("High score is " ++ (show $ (\v -> assert (v == 3243916887) v) highScore))
