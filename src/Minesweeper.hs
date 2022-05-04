module Minesweeper where

import Data.Array ( Ix(inRange, range), Array, array )
import System.Random ( StdGen, Random(randomR) )
import GHC.RTS.Flags (ProfFlags(doHeapProfile))
import Data.List ((\\), sort)

--each cell can be a bomb or value, values have a val and both bombs and values are either flagged or not and either hidden or not
data CellData = Bomb { flag :: Bool, hidden :: Bool } | Value { val :: Int, flag :: Bool, hidden :: Bool } deriving (Eq,Show,Ord)
type Cell = CellData
--a data type used purely to decide whether the game is won or lost
data Result = Win | Loss deriving (Eq,Show)
data State = Running | GameOver Result deriving (Eq,Show)
type Board = Array (Int,Int) Cell

data Game = Game { gameBoard :: Board
                 , gameState :: State
                 } deriving (Eq,Show)

--board size (it's a square board)
n :: Int
n = 16

--amount of bombs (don't exceed total number of cells on board)
m :: Int
m = 40

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 640

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

--creates a Game type using an array which is generated using the functions below. The game begins in 'running' state
initialGame :: [(Int,Int)] -> Game
initialGame x = Game { gameBoard = array indexRange $ sort $ makeArray x
                     , gameState = Running
                     }

--used for isCoordinateWithinBounds below
indexRange :: ((Int, Int), (Int, Int))
indexRange = ((0, 0), (n - 1, n - 1))

--uses the Array data type to contruct an array of coordinates and cell pairs
makeArray :: [(Int,Int)] -> [((Int,Int),CellData)]
makeArray x = makeValueList (range indexRange \\ x) (range indexRange \\ x) [] ++ makeBombList x

--generates the coordinates that the bombs will be on randomly
makeIntList :: [(Int,Int)] -> StdGen -> StdGen -> [(Int,Int)]
makeIntList x f g
    | length x == m = x
    | (fst (randomR (0,n-1) f),fst (randomR (0,n-1) g)) `notElem` x = 
        makeIntList ((fst (randomR (0,n-1) f),fst (randomR (0,n-1) g)) : x) (snd (randomR (0,n-1) f)) (snd (randomR (0,n-1) g))
    | otherwise = makeIntList x (snd (randomR (0,n-1) f)) (snd (randomR (0,n-1) g))

--makes the list of coordinates and cells of type bomb
makeBombList :: [(Int,Int)] -> [((Int,Int),CellData)]
makeBombList x = zip x (repeat (Bomb False True))

--makes the list of coordinates and cells of type value
makeValueList :: [(Int,Int)] -> [(Int,Int)] -> [((Int,Int),CellData)] -> [((Int,Int),CellData)]
makeValueList [] _ y = y
makeValueList (x:xs) x2 y = makeValueList xs x2 ((x, Value (calculateAdjacent x x2 (-1) (-1) 0) False True) : y)
    where
        --x is the middle tile, y is the list of coordinates for only Value type cells, i and j are incrementors for recursion, z is the bomb count
        calculateAdjacent :: (Int,Int) -> [(Int,Int)] -> Int -> Int -> Int -> Int
        calculateAdjacent (x1,x2) y i j z
            | i == (-1) && j == 2 = z
            | i == 2 = calculateAdjacent (x1,x2) y (-1) (j+1) z
            | isCoordinateWithinBounds (x1 + i,x2 + j) = if (x1 + i,x2 + j) `notElem` y 
                                                         then calculateAdjacent (x1,x2) y (i+1) j (z+1) 
                                                         else calculateAdjacent (x1,x2) y (i+1) j z
            | otherwise = calculateAdjacent (x1,x2) y (i+1) j z

--checks whether given coordinates are within the defined bounds of the board
isCoordinateWithinBounds :: (Int, Int) -> Bool
isCoordinateWithinBounds = inRange ((0,0),(n-1,n-1))