module Day12 where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Control.Exception

data State = State{ first :: Int, pots :: [Char] } deriving (Show)

addRule :: M.Map [Char] Char -> String -> M.Map [Char] Char
addRule oldRules ruleStr = newRules where
    rule = take 5 ruleStr
    output = last ruleStr
    newRules = M.insert rule output oldRules

parseInput :: String -> (State, M.Map [Char] Char)
parseInput input = (state, rules) where
    ls = lines input
    state = State 0 . drop 15 $ head ls
    rules = foldl addRule M.empty $ drop 2 ls

-- | Goes through the input and generates the corresponging set of pots
-- Input pot row should be padded with "...." on both sides to handle edges
addPots :: M.Map [Char] Char -> [Char] -> [Char]
addPots rules padded = recurse padded where
    recurse :: [Char] -> [Char]
    recurse [] = ""
    recurse refPots = pot ++ recurse (tail refPots) where
            pot = case M.lookup (take 5 refPots) rules of
                    Just output -> [output]
                    Nothing -> "."


-- | Iterates from initial state for given number of generations
getGen :: Int -> M.Map [Char] Char -> State -> State
getGen 0 rules state = state
getGen gen rules (State firstI pots) = getGen (gen - 1) rules newState where
    paddedPots = "...." ++ pots ++ "...."
    rawPots = addPots rules paddedPots
    firstOffset = fromJust $ L.elemIndex '#' rawPots
    lastOffset = length rawPots - (fromJust . L.elemIndex '#' $ reverse rawPots)
    newFirst = firstI + firstOffset - 2
    newState = State newFirst . take (lastOffset - firstOffset) $ drop firstOffset rawPots

-- | Sums indices of pots that have a plant in them
sumPots :: State -> Int
sumPots (State firstI pots) = result where
    potTransform = (\pot -> case pot of
                                '#' -> 1
                                '.' -> 0)
    potAcc = (\acc (i, pot) -> acc + i * pot)
    result = foldl potAcc 0 . zip [firstI..] $ map potTransform pots

d12p1 :: String -> IO ()
d12p1 input = do
    let (iState, rules) = parseInput input
    let result = (\v -> assert (v == 6201) v) . sumPots $ getGen 20 rules iState
    putStrLn ("Sum of all indices containing a plant is " ++ (show result))

d12p2 :: String -> IO ()
d12p2 input = do
    let (iState, rules) = parseInput input
    -- This is not universal
    -- My input stabilizes to a "full" row at gen 92, continues moving right one pot at a time
    let (State sI sPots) = getGen 92 rules iState
    let result = (\v -> assert (v == 9300000001023) v) $ sumPots (State (sI + (50000000000 - 92)) sPots)
    putStrLn ("Sum of all indices containing a plant is " ++ (show result))
