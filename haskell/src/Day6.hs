module Day6 where

import Control.Exception
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Sequence

data Coord = Coord Int Int

parseCoord :: String -> Coord
parseCoord line = do
    let cs = splitOn ", " line
    let x = read $ cs !! 0 :: Int
    let y = read $ cs !! 1 :: Int
    Coord x y

parseInput :: String -> [Coord]
parseInput input = map parseCoord $ lines input

-- |Returns manhattan distance between two coordinates
dist :: Coord -> Coord -> Int
dist (Coord x1 y1) (Coord x2 y2) =
    (abs (x2 - x1)) + (abs (y2 - y1))

-- |Returns single closest Cord in list for a Coord or Nothing
closestCoord :: [(Int, Int)] -> Coord -> [(Int, Coord)] -> Maybe Int
closestCoord ((ci, _):[]) (Coord x y) [] = Just ci
closestCoord ((ci, _):rest) (Coord x y) [] = Nothing
closestCoord ((ci, cd):restClosest) c ((ni, nc):restCoords) = do
    let nd = (dist c nc)
    if nd < cd
        then (closestCoord [(ni, nd)] c restCoords)
        else if nd == cd
            then (closestCoord ([(ci, cd)] ++ restClosest ++ [(ni, nd)]) c restCoords)
            else (closestCoord ([(ci, cd)] ++ restClosest) c restCoords)
closestCoord [] c ((ni, nc):restCoords) =
    (closestCoord [(ni, (dist c nc))] c restCoords)

-- |Checks if Coord is on the edge of coordinate space
onEdge :: Int -> Int -> Coord -> Bool
onEdge h w (Coord x y) = x == 0 || y == 0 || x == w - 1 || y == h - 1

-- |Updates the finite area corresponding to the given Coord
-- Coord with multiple closest coordinates is not added
-- Coord on edge sets the corresponding area as infinite
addCoordFinite :: (Coord -> Bool) -> [(Int, Coord)] -> Seq Int -> Coord -> Seq Int
addCoordFinite edgeCheck coords oldAreas c = do
    case closestCoord [] c coords of
        Just i -> 
            if index oldAreas i /= -1
                then if edgeCheck c
                    then update i (-1) oldAreas
                    else adjust (\v -> v + 1) i oldAreas
                else oldAreas
        Nothing -> oldAreas

-- |Updates finite areas with a row of Coords
addRowFinite :: (Coord -> Bool) -> Int -> [(Int, Coord)] -> Seq Int -> Int -> Seq Int
addRowFinite edgeCheck w coords oldAreas y = do
    foldl (\acc x -> addCoordFinite edgeCheck coords acc (Coord x y)) oldAreas [0..w]

-- |Calculates the size of the largest finite area defined by the input Coords
largestFinite :: [Coord] -> Int
largestFinite coords = do
    let (Coord w _) = maximumBy (\(Coord x1 _) (Coord x2 _) -> compare x1 x2) coords
    let (Coord _ h) = maximumBy (\(Coord _ y1) (Coord _ y2) -> compare y1 y2) coords
    let enumCoords = Prelude.zip [0..] coords
    let emptyAreas = fromList . Data.List.take (Data.List.length coords) $ repeat 0 
    let acc = addRowFinite (onEdge h w) w enumCoords
    maximum $ foldl acc emptyAreas [0..h]

d6p1 :: String -> IO ()
d6p1 input = do
    let result = (\v -> assert (v == 3909) v) . largestFinite $ parseInput input
    putStrLn ("Size of largest finite area is " ++ (show result))

-- |Increments input oldAcc according to the distance rule
addCoordDist :: [Coord] -> Coord -> Int -> Int
addCoordDist coords coord oldAcc = do
    let d = foldl (\acc c -> acc + (dist coord c)) 0 coords
    if d < 10000
        then oldAcc + 1
        else oldAcc

-- |Increments rowAcc for points on row according to the distance rule
addRowDist :: Int -> [Coord] -> Int -> Int -> Int
addRowDist w coords rowAcc y = 
    foldl (\acc x -> addCoordDist coords (Coord x y) acc) rowAcc [0..w]

-- |Returns the number of points that inside the region defined by the distance rule
regionSize :: [Coord] -> Int
regionSize coords = do
    let (Coord w _) = maximumBy (\(Coord x1 _) (Coord x2 _) -> compare x1 x2) coords
    let (Coord _ h) = maximumBy (\(Coord _ y1) (Coord _ y2) -> compare y1 y2) coords
    let acc = addRowDist w coords
    foldl acc 0 [0..h]

d6p2 :: String -> IO ()
d6p2 input = do
    let result = (\v -> assert (v == 36238) v) . regionSize $ parseInput input
    putStrLn ("Size of the region is " ++ (show result))
