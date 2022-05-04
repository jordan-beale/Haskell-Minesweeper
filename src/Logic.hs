module Logic where

import Graphics.Gloss.Interface.Pure.Game
    ( Key(MouseButton),
      KeyState(Down),
      MouseButton(RightButton, LeftButton),
      Event(EventKey) )
import Data.Array ( Ix(range), (!), (//), elems )
import Data.List (sort)

import Minesweeper
    ( cellHeight,
      cellWidth,
      indexRange,
      isCoordinateWithinBounds,
      screenHeight,
      screenWidth,
      Board,
      Cell,
      CellData(..),
      Game(..),
      Result(Loss, Win),
      State(GameOver, Running),
      CellData(Value) )

--converts a mouse position to the respective coordinates
mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )

checkIfValueRevealed :: CellData -> Bool
checkIfValueRevealed (Value _ _ False) = True
checkIfValueRevealed Value {} = False
checkIfValueRevealed (Bomb _ _) = True

checkIfValueAllRevealed :: [CellData] -> Bool
checkIfValueAllRevealed = all checkIfValueRevealed

checkGameOver :: Game -> Game
checkGameOver (Game board state)
    | checkIfValueAllRevealed $ elems board =
        (Game board state) { gameState = GameOver Win}
    | otherwise = Game board state

--four functions used exclusively to reveal all the adjacent zero and bordering non-zero tiles when a zero tile is revealed
checkIfNeighbouringZero :: (Int,Int) -> Board -> Bool
checkIfNeighbouringZero (x,y) board = any (\z -> (if isCoordinateWithinBounds z then board ! z else Value 0 True False ) == Value 0 False False)
                                        [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]

getValue :: CellData -> Int
getValue (Value x _ _) = x
getValue (Bomb _ _) = 0

cleanUpRemoveZeros :: [(Int,Int)] -> Game -> Game
cleanUpRemoveZeros [] (Game board state) = Game board state
cleanUpRemoveZeros (x:xs) (Game board state)
    | checkIfNeighbouringZero x board = cleanUpRemoveZeros xs ((Game board state) { gameBoard = board // [(x, Value {val = getValue $ board ! x, flag=False, hidden=False})] } )
    | otherwise = cleanUpRemoveZeros xs (Game board state)

removeZeros :: Board -> (Int,Int) -> CellData -> CellData -> Board
removeZeros board coordinate target replacement =
    if target == replacement then
        board
    else
        let revealZero = if isCoordinateWithinBounds coordinate then board // [(coordinate, replacement)] else board
            validNeighbours = filter (isCoordinateWithinBounds `and'` sameAsTarget) neighbours in
            foldl (\board coordinate -> removeZeros board coordinate target replacement) revealZero validNeighbours

            where
                neighbours = let (x,y) = coordinate in [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]
                sameAsTarget coordinate = board ! coordinate == target
                and' f g x = f x && g x

--function to reveal a cell and update the boardState within the game repectively
revealCell :: Game -> (Int,Int) -> Int -> Game
revealCell (Game board state) cellCoordinate x
    | isCoordinateWithinBounds cellCoordinate && (board ! cellCoordinate) == Bomb { flag = False, hidden = True } =
        (Game board state) { gameBoard = board // [(cellCoordinate, Bomb { flag = True, hidden = True })], gameState = GameOver Loss }
    | isCoordinateWithinBounds cellCoordinate && x == 0 && (board ! cellCoordinate) == Value { flag = False, hidden = True, val = x } =
        checkGameOver
        $ cleanUpRemoveZeros (range indexRange)
        $ Game (removeZeros board cellCoordinate (Value 0 False True) (Value 0 False False)) state
    | isCoordinateWithinBounds cellCoordinate && (board ! cellCoordinate) == Value { flag = False, hidden = True, val = x } =
        checkGameOver
        $ (Game board state) { gameBoard = board // [(cellCoordinate, Value { flag = False, hidden = False, val = x })] }
    | otherwise = Game board state

--handles the even when a flag is either placed or removed from the board, again updating the boardState respectively
flagEvent :: Game -> (Int,Int) -> Int -> Game
flagEvent (Game board state) cellCoordinate x
    | isCoordinateWithinBounds cellCoordinate && (board ! cellCoordinate) == Bomb { flag = False, hidden = True } =
        (Game board state) { gameBoard = board // [(cellCoordinate, Bomb { flag = True, hidden = True })] }
    | isCoordinateWithinBounds cellCoordinate && (board ! cellCoordinate) == Bomb { flag = True, hidden = True } =
        (Game board state) { gameBoard = board // [(cellCoordinate, Bomb { flag = False, hidden = True })] }
    | isCoordinateWithinBounds cellCoordinate && (board ! cellCoordinate) == Value { flag = False, hidden = True, val = x} =
        (Game board state) { gameBoard = board // [(cellCoordinate, Value { flag = True, hidden = True, val = x})] }
    | isCoordinateWithinBounds cellCoordinate && (board ! cellCoordinate) == Value { flag = True, hidden = True, val = x} =
        (Game board state) { gameBoard = board // [(cellCoordinate, Value { flag = False, hidden = True, val = x})] }
    | otherwise = Game board state

--handles the user inputs and updates the game accordingly. left click attempts to reveal a cell and right click attempts to either add or remove a flag
transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Down _ mousePos) game =
    case gameState game of
      Running -> if any (/= game) ([revealCell game (mousePosAsCellCoord mousePos) x | x <- [0 .. 8]])
                 then head $ filter (/= game) [revealCell game (mousePosAsCellCoord mousePos) x | x <- [0 .. 8]]
                 else game
      GameOver _ -> game
transformGame (EventKey (MouseButton RightButton) Down _ mousePos) game =
    case gameState game of
      Running -> if any (/= game) ([flagEvent game (mousePosAsCellCoord mousePos) x | x <- [0 .. 8]])
                 then head $ filter (/= game) [flagEvent game (mousePosAsCellCoord mousePos) x | x <- [0 .. 8]]
                 else game
      GameOver _ -> game
transformGame _ game = game
