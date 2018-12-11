module Day11 where

import qualified Data.Vector as V
import Data.Ord
import Control.Exception

-- | Generates square coordinates for range [1, size]
genCoords :: Int -> V.Vector (V.Vector (Int, Int))
genCoords size = coords where
    dim = V.enumFromTo 1 size
    coords = V.map (\y -> V.zip dim (V.replicate size y)) dim

-- | Generates 300x300 grid of powercells with powers according to given serial number
genGrid :: Int -> V.Vector (V.Vector Int)
genGrid serial = grid where
    coords = genCoords 300
    grid = V.map (\row -> V.map powerLevel row) coords where
        powerLevel :: (Int, Int) -> Int
        powerLevel (x, y) = (mod (quot power 100) 10) - 5 where
            rackID = x + 10
            power = (rackID * y + serial) * rackID

-- | Calculates sum of a square power cell of given size using pre-calculated power
-- for size - 1 with the same top left corner
sqPower :: Int -> V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> (Int, Int) -> Int
sqPower size previous grid (x, y) = power where
    (yb, ye) = (y - 1, y + size - 2)
    (xb, xe) = (x - 1, x + size - 2)
    pSum = (previous V.! yb) V.! xb
    singles = sum . map (\(xx, yy) -> (grid V.! yy) V.! xx) $ zip (repeat xe) [yb..(ye - 1)]
    nextRow = V.sum . V.take size . V.drop (x - 1) $ grid V.! ye
    power = pSum + singles + nextRow

-- | Finds square with largest power up to the given max size by constructing the solutions
-- for individual squares bottom up
findBestSq :: Int -> V.Vector (V.Vector Int) -> (Int, (Int, Int, Int))
findBestSq maxSize grid = best where
    best = bottomUp 2 grid (0, (0, 0, 0)) where
    bottomUp :: Int -> V.Vector (V.Vector Int) -> (Int, (Int, Int, Int)) -> (Int, (Int, Int, Int))
    bottomUp size previous best
        | size > maxSize = best
        | otherwise =  bottomUp (size + 1) ((V.map . V.map) fst sums) newBest where
            coords = genCoords (300 - size + 1)
            sums = (V.map . V.map) (\(x, y) -> (sqPower size previous grid (x, y), (x, y, size))) coords
            sizeBest = V.maximumBy (comparing fst) $ V.map (V.maximumBy (comparing fst)) sums
            newBest = if (fst sizeBest) > (fst best)
                        then sizeBest
                        else best

d11p1 :: String -> IO ()
d11p1 input = do
    let serial = read input :: Int
    let (_, (x, y, _)) = findBestSq 3 $ genGrid serial
    let result = (\v -> assert (v == "235,87") v) $ (show x) ++ "," ++ (show y)
    putStrLn ("3x3 cell with largest power is " ++ result)

d11p2 :: String -> IO ()
d11p2 input = do
    let grid = genGrid $ read input
    let (_, (x, y, size)) = findBestSq 300 grid
    let result = (\v -> assert (v == "234,272,18") v) $ (show x) ++ "," ++ (show y) ++ "," ++ (show size)
    putStrLn ("Cell with largest power is " ++ result)
