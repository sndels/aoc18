module Day8 where

import qualified Data.List as List
import Control.Exception

data Node = Node{ meta :: [Int], children :: [Node] }

parseInput :: String -> [Int]
parseInput input = map (\i -> read i) $ words input

parseChildren :: ([Node], [Int]) -> Int -> ([Node], [Int])
parseChildren (children, input) 0 = (children, input)
parseChildren (children, input) n = do
    let (child, restInput) = parseNode input
    parseChildren (children ++ [child], restInput) (n - 1)

parseNode :: [Int] -> (Node, [Int])
parseNode input = do
    let nChildren = input !! 0
    let nMeta = input !! 1
    let (children, restInput) = if nChildren > 0
                                    then parseChildren ([], drop 2 input) nChildren
                                    else ([], drop 2 input)
    let meta = foldl (\metacc m -> metacc ++ [m]) [] $ take nMeta restInput
    (Node meta children, drop nMeta restInput)

sumMetas :: Node -> Int
sumMetas (Node meta []) = sum meta
sumMetas (Node meta children ) =
    sum meta + foldl (\acc child -> acc + sumMetas child) 0 children

d8p1 :: String -> IO ()
d8p1 input = do
    let (tree, _) = parseNode $ parseInput input
    let metasum = (\v -> assert (v == 41556) v) $ sumMetas tree
    putStrLn ("Sum of all metadatas is " ++ show metasum)

childValue :: Int -> [Node] -> Int
childValue i children =
    if i > 0 && i <= length children
        then nodeValue $ children !! (i - 1)
        else 0

nodeValue :: Node -> Int
nodeValue (Node meta []) = sum meta
nodeValue (Node meta children) =
    foldl (\acc m -> acc + childValue m children) 0 meta

d8p2 :: String -> IO ()
d8p2 input = do
    let (tree, _) = parseNode $ parseInput input
    let rootValue = (\v -> assert (v == 16653) v) $ nodeValue tree
    putStrLn ("Value of the root node is " ++ show rootValue)
