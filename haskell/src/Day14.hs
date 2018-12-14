module Day14 where

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Exception
import Data.Maybe
import Debug.Trace

data State = State {f :: Int, s :: Int, recipes :: S.Seq Int}

genRecipes :: Int -> S.Seq Int
genRecipes num = gen (State 0 1 (S.fromList [3, 7])) where
    gen :: State -> S.Seq Int
    gen (State f s recipes)
        | length recipes >= num = recipes
        | otherwise = gen (State newF newS newRecipes) where
            fVal = fromJust $ recipes S.!? f
            sVal = fromJust $ recipes S.!? s
            rSum = fVal + sVal
            newRecipes = if rSum < 10
                            then recipes S.|> rSum
                            else (recipes S.|> fDigit) S.|> sDigit where
                                fDigit = quot rSum 10
                                sDigit = mod rSum 10
            newF = mod (f + fVal + 1) (length newRecipes)
            newS = mod (s + sVal + 1) (length newRecipes)

d14p1 :: String -> IO ()
d14p1 input = do
    let n = read input :: Int
    let recipes = F.toList . S.drop n $ genRecipes (n + 10)
    let recipeStr = (\v -> assert (v == "2103141159") v) . concat $ map show recipes
    putStrLn ("Scores for the next ten recipes are " ++ recipeStr)

findRecipe :: String -> S.Seq Int -> Int
findRecipe recipe recipes = recur 0 recipe (concat . map show $ F.toList recipes) where
    recur :: Int -> String -> String -> Int
    recur n recipe input
        | recipe == (take (length recipe) input) = n
        | otherwise = recur (n + 1) recipe (tail input)

d14p2 :: String -> IO ()
d14p2 input = do
    let recipe = init input
    -- TODO: How to best define lazy generation in this case
    let n = (\v -> assert (v == 20165733) v) . findRecipe recipe $ genRecipes 30000000
    putStrLn ((show n) ++ " recipes appear before")
