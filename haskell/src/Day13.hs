module Day13 where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector as V
import Control.Exception
import Data.Maybe
import Data.Ord

data Cart = Cart { cartDir :: Char, lastTurn :: Char , cartPos :: (Int, Int) } deriving (Show)
data State = State { stateCarts :: [Cart], stateTracks :: V.Vector (V.Vector Char) }

accCarts :: [Cart] -> (Int, String) -> [Cart]
accCarts carts (y, line) = carts ++ newCarts where
    newCarts = foldl accCart [] $ zip [0..] line where
        accCart :: [Cart] -> (Int, Char) -> [Cart]
        accCart carts (x, c)
            | c == '<' || c == '>' || c == '^' || c == 'v' = carts ++ [(Cart c 'r' (y, x))]
            | otherwise = carts

pruneCarts :: Char -> Char
pruneCarts c
    | c == '<' || c == '>' = '-'
    | c == '^' || c == 'v' = '|'
    | otherwise = c

parseInput :: String -> State
parseInput input = State carts tracks where
    iLines = lines input
    carts = foldl accCarts [] $ zip [0..] iLines
    tracks = V.fromList . map V.fromList $ (map . map) pruneCarts iLines

turnCart :: Char -> Char -> (Char, Char)
turnCart dir lastTurn = (newDir, turn) where
    turn = case lastTurn of
                'l' -> 's'
                's' -> 'r'
                'r' -> 'l'
    newDir = case dir of
                '<' -> case turn of
                            'l' -> 'v'
                            'r' -> '^'
                            's' -> '<'
                '^' -> case turn of
                            'l' -> '<'
                            'r' -> '>'
                            's' -> '^'
                '>' -> case turn of
                            'l' -> '^'
                            'r' -> 'v'
                            's' -> '>'
                'v' -> case turn of
                            'l' -> '>'
                            'r' -> '<'
                            's' -> 'v'

moveCart :: V.Vector (V.Vector Char) -> Cart -> Cart
moveCart tracks (Cart dir lastTurn (y, x)) =
    case dir of
        '<' -> Cart ndir turn (y, nx) where
                nx = x - 1
                (ndir, turn) = case (tracks V.! y) V.! nx of
                                '\\' -> ('^', lastTurn)
                                '/' -> ('v', lastTurn)
                                '+' -> turnCart dir lastTurn
                                otherwise -> ('<', lastTurn)
        '^' -> Cart ndir turn (ny, x) where
                ny = y - 1
                (ndir, turn) = case (tracks V.! ny) V.! x of
                                '\\' -> ('<', lastTurn)
                                '/' -> ('>', lastTurn)
                                '+' -> turnCart dir lastTurn
                                otherwise -> ('^', lastTurn)
        '>' -> Cart ndir turn (y, nx) where
                nx = x + 1
                (ndir, turn) = case (tracks V.! y) V.! nx of
                                '\\' -> ('v', lastTurn)
                                '/' -> ('^', lastTurn)
                                '+' -> turnCart dir lastTurn
                                otherwise -> ('>', lastTurn)
        'v' -> Cart ndir turn (ny, x) where
                ny = y + 1
                (ndir, turn) = case (tracks V.! ny) V.! x of
                                '\\' -> ('>', lastTurn)
                                '/' -> ('<', lastTurn)
                                '+' -> turnCart dir lastTurn
                                otherwise -> ('v', lastTurn)

doesCrash :: (Int, Int) -> [Cart] -> Maybe (Int, Int)
doesCrash pos [] = Nothing
doesCrash pos ((Cart _ _ otherPos):rest) =
    if pos == otherPos
        then Just pos
        else doesCrash pos rest

-- | Steps carts and checks for collisions in between
-- Only handles one collision per tic as that sufficed on my input
stepCarts :: State -> (State, Maybe (Int, Int))
stepCarts (State carts tracks) = recur Nothing [] carts where
    recur :: Maybe (Int, Int) -> [Cart] -> [Cart] -> (State, Maybe (Int, Int))
    recur crash acc [] = (State acc tracks, crash)
    recur crash acc (cart:rest) = recur newCrash (acc ++ [newCart]) rest where
        newCart = if isJust crash && (fromJust crash) == (cartPos cart)
                    then cart
                    else moveCart tracks cart
        newCrash = if isJust crash
                    then crash
                    else doesCrash (cartPos newCart) (acc ++ rest)

-- | Steps carts until a crash is encountered
stepTilCrash :: State -> (Int, Int)
stepTilCrash state = crash where
    ((State carts tracks), maybeCrash) = stepCarts state
    sortedCarts = L.sortBy (comparing cartPos) carts
    crash = if isJust maybeCrash
                then fromJust maybeCrash
                else stepTilCrash (State sortedCarts tracks)

d13p1 :: String -> IO ()
d13p1 input = do
    let initState = parseInput input
    let (y, x) = stepTilCrash initState
    let result = (\v -> assert (v == "94,78") v) $ (show x) ++ "," ++ (show y)
    putStrLn ("First crash at " ++ result)

excludePos :: (Int, Int) -> Cart -> Bool
excludePos pos (Cart _ _ otherPos) = pos /= otherPos

-- | Steps carts until a crash is encountered
stepTilOne :: State -> (Int, Int)
stepTilOne (State ((Cart _ _ pos):[]) _)= pos where
stepTilOne state = pos where
    ((State carts tracks), maybeCrash) = stepCarts state
    sortedCarts = L.sortBy (comparing cartPos) carts
    pos = if isJust maybeCrash
            then stepTilOne (State (filter (excludePos (fromJust maybeCrash)) sortedCarts) tracks)
            else stepTilOne (State sortedCarts tracks)

d13p2 :: String -> IO ()
d13p2 input = do
    let initState = parseInput input
    let (y, x) = stepTilOne initState
    let result = (\v -> assert (v == "26,85") v) $ (show x) ++ "," ++ (show y)
    putStrLn ("Last cart at " ++ result)
