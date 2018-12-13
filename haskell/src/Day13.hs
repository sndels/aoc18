module Day13 where

import qualified Data.Vector as V
import qualified Data.Set as S
import Control.Exception

data Cart = Cart { dir :: Char, lastTurn :: Char , pos :: (Int, Int) } deriving (Show)
data State = State { carts :: [Cart], tracks :: V.Vector (V.Vector Char) }

accCarts :: [Cart] -> (Int, String) -> [Cart]
accCarts carts (y, line) = carts ++ newCarts where
    newCarts = foldl accCart [] $ zip [0..] line where
        accCart :: [Cart] -> (Int, Char) -> [Cart]
        accCart carts (x, c)
            | c == '<' || c == '>' || c == '^' || c == 'v' = carts ++ [(Cart c 'r' (x, y))]
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
moveCart tracks (Cart dir lastTurn (x, y)) =
    case dir of
        '<' -> Cart ndir turn (nx, y) where
                nx = x - 1
                (ndir, turn) = case (tracks V.! y) V.! nx of
                                '\\' -> ('^', lastTurn)
                                '/' -> ('v', lastTurn)
                                '+' -> turnCart dir lastTurn
                                otherwise -> ('<', lastTurn)
        '^' -> Cart ndir turn (x, ny) where
                ny = y - 1
                (ndir, turn) = case (tracks V.! ny) V.! x of
                                '\\' -> ('<', lastTurn)
                                '/' -> ('>', lastTurn)
                                '+' -> turnCart dir lastTurn
                                otherwise -> ('^', lastTurn)
        '>' -> Cart ndir turn (nx, y) where
                nx = x + 1
                (ndir, turn) = case (tracks V.! y) V.! nx of
                                '\\' -> ('v', lastTurn)
                                '/' -> ('^', lastTurn)
                                '+' -> turnCart dir lastTurn
                                otherwise -> ('>', lastTurn)
        'v' -> Cart ndir turn (x, ny) where
                ny = y + 1
                (ndir, turn) = case (tracks V.! ny) V.! x of
                                '\\' -> ('>', lastTurn)
                                '/' -> ('<', lastTurn)
                                '+' -> turnCart dir lastTurn
                                otherwise -> ('v', lastTurn)

stepCarts :: State -> State
stepCarts (State carts tracks) = State newCarts tracks where
    newCarts = map (moveCart tracks) carts

findCrash :: S.Set (Int, Int) -> [Cart] -> Maybe (Int, Int)
findCrash _ [] = Nothing
findCrash seenCarts ((Cart _ _ pos):rest)
    | S.member pos seenCarts = Just pos
    | otherwise = findCrash (S.insert pos seenCarts) rest

-- Steps carts instantly instead of sequential steps in a tick with collision
-- checks in between
stepTilCrash :: State -> (Int, Int)
stepTilCrash state = crash where
    (State carts tracks) = stepCarts state
    crash = case findCrash S.empty carts of
                    Just c -> c
                    Nothing -> stepTilCrash (State carts tracks)

d13p1 :: String -> IO ()
d13p1 input = do
    let initState = parseInput input
    let (x, y) = stepTilCrash initState
    putStrLn ("First crash at " ++ (show x) ++ "," ++ (show y))

d13p2 :: String -> IO ()
d13p2 input = do
    putStrLn ("unimplemented")
