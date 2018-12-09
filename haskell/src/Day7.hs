module Day7 where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Char
import Data.Ord
import Data.Functor
import Control.Exception
import Data.List.Split
import Data.Maybe
import Debug.Trace

data Edge = Edge Char Char
data State = State { workers :: Seq.Seq (Char, Int),
                     steps :: [Char],
                     prereqs :: Map.Map Char [Char],
                     done :: [Char] }

parseEdge :: [Char] -> Edge
parseEdge line = do
    let cs = splitOn " " line
    let s = head $ cs !! 1
    let t = head $ cs !! 7
    Edge s t

parseInput :: [Char] -> [Edge]
parseInput input =
    map parseEdge $ lines input

addEdge :: Map.Map Char [Char] -> Edge -> Map.Map Char [Char]
addEdge adjacency (Edge s t) =
    case Map.lookup s adjacency of
        Just es -> Map.insert s (List.reverse . List.sort $ es ++ [t]) adjacency
        Nothing -> Map.insert s [t] adjacency

genAdjacency :: [Edge] -> Map.Map Char [Char]
genAdjacency edges =
    foldl addEdge Map.empty edges

-- | DFS returning visited for bookeeping and reverse post order
dfs :: Map.Map Char [Char] -> Set.Set Char -> [Char] -> (Set.Set Char, [Char])
dfs graph visited [] = (visited, [])
dfs graph visited (node:rest) = 
    if Set.member node visited
        then dfs graph visited rest
        else do
    let (nVisited, nPostOrder) = case Map.lookup node graph of
                                    Just neighbours -> dfs graph (Set.insert node visited) neighbours
                                    Nothing -> (Set.insert node visited, [])
    let (rVisited, rPostOrder) = dfs graph nVisited rest
    (rVisited, rPostOrder ++ [node] ++ nPostOrder)

d7p1 :: [Char] -> IO ()
d7p1 input = do
    let adjacency = genAdjacency $ parseInput input
    let sortedSteps = List.reverse . List.sort $ Map.keys adjacency
    let (visited, revPostOrder) = dfs adjacency Set.empty sortedSteps
    let order = (\v -> assert (v == "CGKMUWXFAIHSYDNLJQTREOPZBV") v) revPostOrder
    putStrLn $ "Step order should be " ++ order

time :: Char -> Int
time step = (ord step) - (ord 'A') + 61

assignWorker :: State -> Int -> State
assignWorker (State workers (step:restSteps) prereqs done) worker = do
    let ps = case Map.lookup step prereqs of-- TODO: This is an infinite loop?
                Just ss -> ss
                Nothing -> []
    if all (\p -> List.elem p done) ps
        then do
            let newWorkers = Seq.update worker (step, (time step)) workers
            let newDone = done ++ [fst $ Seq.index workers worker]
            (State newWorkers restSteps prereqs newDone)
        else (State workers (step:restSteps) prereqs done)

assignWorkers :: State -> [Int] -> State
assignWorkers state free = foldl assignWorker state free

constructTime :: Map.Map Char [Char] -> [Char] -> Int
constructTime prereqs steps =
    constructTime' (State (Seq.fromList [(' ', 0), (' ', 0)]) steps prereqs "") 0
    where constructTime' :: State -> Int -> Int
          constructTime' (State workers [] _ _) total =
            total + (snd $ List.maximumBy (comparing snd) workers)
          constructTime' state total = do
            let free = Seq.findIndicesL (\w -> snd w <= 0) $ (workers state)
            let (State newWorkers newSteps prereqs newDone) = if null free
                                                                then state
                                                                else assignWorkers state free
            let minTime = snd $ List.minimumBy (comparing snd) newWorkers
            let decWorkers = (fmap (\w -> (fst w, snd w - minTime)) newWorkers)
            constructTime' (State decWorkers newSteps prereqs newDone) (total + minTime)

d7p2 :: [Char] -> IO ()
d7p2 input = do
    let testInput = "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin."
    let prereqs = genAdjacency . map (\(Edge s t) -> Edge t s) $ parseInput testInput
    let seconds = constructTime prereqs "CABDFE"
    putStrLn (show seconds)
