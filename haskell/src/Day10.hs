module Day10 where

import qualified Data.List as List
import qualified Data.Set as Set
import Data.Char (isDigit)

data Point = Point{ x :: Int, y :: Int, vx :: Int, vy :: Int } deriving (Show)

pruneLine :: String -> String
pruneLine [] = []
pruneLine (c:rest)
    | c == '-' || isDigit c = [c] ++ pruneLine rest
    | c == ',' || c == '<' = [' '] ++ pruneLine rest
    | otherwise = pruneLine rest

parsePoint :: String -> Point
parsePoint input = point where
    (x:y:vx:vy:_) = map read . words $ pruneLine input
    point = Point x y vx vy

parseInput :: String -> [Point]
parseInput input = map parsePoint $ lines input

stepPoint :: Point -> Point
stepPoint (Point x y vx vy) = Point (x + vx) (y + vy) vx vy

-- | Simulates point cloud until message converges
ffwd :: [Point] -> [Point]
ffwd points = head . dropWhile (\ps -> (lDim $ yDim ps) > 10) $ iterate (map stepPoint) points

-- | Returns limits of the point cloud in Y
yDim :: [Point] -> (Int, Int)
yDim points = (low, high) where
    trackY = (\(low, high) (Point _ y _ _) -> ((min low y), (max high y)))
    (low, high) = foldl trackY (1000, (-1000)) points

-- | Returns limits of the point cloud in X
xDim :: [Point] -> (Int, Int)
xDim points = (low, high) where
    trackX = (\(low, high) (Point x _ _ _) -> ((min low x), (max high x)))
    (low, high) = foldl trackX (1000, (-1000)) points

lDim :: (Int, Int) -> Int
lDim (low, high) = high - low + 1

-- | Converts point cloud to a set of coordinates for faster queries
posSet :: [Point] -> Set.Set (Int, Int)
posSet points = foldl (\set (Point x y _ _) -> Set.insert (x, y) set) Set.empty points

-- | Renders message to multiline string
message :: [Point] -> String
message points = output where
    positions = posSet points
    (lY, hY) = yDim points
    (lX, hX) = xDim points
    output = render lX lY where
        render :: Int -> Int -> String
        render x y
            | y > hY = ""
            | x > hX = "\n" ++ render lX (y + 1)
            | Set.member (x, y) positions = "#" ++ render (x + 1) y
            | otherwise = "." ++ render (x + 1) y

d10p1 :: String -> IO ()
d10p1 input = do
    putStrLn (message . ffwd . parseInput $ input)

-- | ffwd that keeps track of seconds simulated
ffwdWithSecond :: [Point] -> (Int, [Point])
ffwdWithSecond points = head . dropWhile (\(_, ps) -> (lDim $ yDim ps) > 10) . zip [0..] $ iterate (map stepPoint) points

d10p2 :: String -> IO ()
d10p2 input = do
    let (seconds, _) = ffwdWithSecond . parseInput $ input
    putStrLn ("Message appears after " ++ (show seconds) ++ " seconds")
