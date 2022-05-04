module Rendering where

import Graphics.Gloss
    ( black,
      blue,
      greyN,
      color,
      orange,
      line,
      pictures,
      rectangleSolid,
      rotate,
      text,
      translate,
      makeColor,
      makeColorI,
      Display(InWindow),
      Color,
      Picture(Blank, Circle, Scale, Text) )
import Data.Array ( assocs )

import Minesweeper
    ( cellHeight,
      cellWidth,
      n,
      screenHeight,
      screenWidth,
      Board,
      Cell,
      CellData(Value, Bomb, flag, hidden),
      Game(gameState, gameBoard),
      Result(..),
      State(GameOver, Running) )

window :: Display
window = InWindow "Mλnesweeper" (screenWidth,screenHeight) (10,10)

--colours used for varius pictures throughout
backgroundColour :: Color
backgroundColour = makeColor 0 0 0 255
white = makeColorI 255 255 255 255
red   = makeColorI 255 50 50 255
darkGrey = makeColorI 100 100 100 255

--the white border for the board
grid :: Picture
grid =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
                              , (i * cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeight)
                              , (fromIntegral screenWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral n]

--messages for when the game has ended
winEndGameMessage :: Picture
winEndGameMessage = pictures [translate 50 380 $ color white $ Scale 1 1 $ text "You won!", 
                      translate 120 190 $ color white $ Scale 0.5 0.5 $ text "ESC to exit"]
lossEndGameMessage = pictures [translate 50 380 $ color white $ Scale 1 1 $ text "You lost!", 
                      translate 120 190 $ color white $ Scale 0.5 0.5 $ text "esc to exit"]

--two functions to format the cells into the right place on the board
snapPictureToCell :: Picture -> (Int, Int) -> Picture
snapPictureToCell picture (row, column) = translate x y picture
    where
        x = fromIntegral column * cellWidth + cellWidth * 0.5
        y = fromIntegral row * cellHeight + cellHeight * 0.5

cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
    pictures
    $ map (snapPictureToCell cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

--three functions each generating the flag, bomb and value 'sprites' respectively
flaggedCell :: Picture
flaggedCell = color red $ pictures [ rotate 45.0 $ rectangleSolid side 10.0
                 , rotate (-45.0) $ rectangleSolid side 10.0
                 ]
    where side = min cellWidth cellHeight * 0.75

bombCell :: Picture 
bombCell = color red $ Circle radius
    where
        radius = min cellWidth cellHeight * 0.25

notHiddenValueCell :: CellData -> Picture
notHiddenValueCell (Bomb _ _) = color orange $ Text "error"
notHiddenValueCell (Value val _ _ ) = color orange $ Scale 0.15 0.15 (Text $ show val)

--nine functions which use record pattern matching of a sort to check what should be displayed for the cells of the type matched
flaggedBombCells :: Board -> Picture 
flaggedBombCells board = cellsOfBoard board (Bomb { flag = True, hidden = True}) bombCell
notFlaggedBombCells board = cellsOfBoard board (Bomb { flag = False, hidden = True}) bombCell

flaggedHiddenBombCells :: Board -> Picture
flaggedHiddenBombCells board = cellsOfBoard board (Bomb True True) flaggedCell
notFlaggedHiddenBombCells board = cellsOfBoard board (Bomb False True) Blank

notFlaggedNotHiddenValueCells :: Board -> Int -> Picture
notFlaggedNotHiddenValueCells board x = cellsOfBoard board (Value x False False) (notHiddenValueCell (Value x False False))

flaggedHiddenValueCells2 :: Board -> Int -> Picture
flaggedHiddenValueCells2 board x = cellsOfBoard board (Value x True True) (notHiddenValueCell (Value x False False))
notFlaggedHiddenValueCells2 board x = cellsOfBoard board (Value x False True) (notHiddenValueCell (Value x False False))

flaggedHiddenValueCells :: Board -> Int -> Picture
flaggedHiddenValueCells board x = cellsOfBoard board (Value x True True) flaggedCell
notFlaggedHiddenValueCells board x = cellsOfBoard board (Value x False True) Blank

--generates the picture to be displayed when the game is over. It reveales all tiles and puts some formatted text ontop with some banners for clarity and aesthetic
gameoverBoardPicture :: Board -> Result -> Picture
gameoverBoardPicture board Win = pictures $ listOfPictures board
                                ++ [winEndGameMessage]
gameoverBoardPicture board Loss = pictures $ listOfPictures board
                                ++ [lossEndGameMessage]
--helper function to remove repetition in code
listOfPictures :: Board -> [Picture]
listOfPictures board = color white grid 
                        : flaggedBombCells board 
                        : notFlaggedBombCells board 
                        : [notFlaggedNotHiddenValueCells board x | x <- [0 .. 8]]
                        ++ [flaggedHiddenValueCells2 board x | x <- [0 .. 8]]
                        ++ [notFlaggedHiddenValueCells2 board x | x <- [0 .. 8]]
                        ++ [translate 320 427 (color black (rectangleSolid 640 150)), translate 320 213 (color black (rectangleSolid 640 100))]
                        ++ [translate 320 505 (color white (rectangleSolid 640 10)), translate 320 355 (color white (rectangleSolid 640 10))]
                        ++ [translate 320 262 (color white (rectangleSolid 640 10)), translate 320 165 (color white (rectangleSolid 640 10))]
                                
--generates the picture for when the game is running
runningBoardPicture :: Board -> Picture
runningBoardPicture board = pictures $ color white grid
                           : flaggedHiddenBombCells board
                           : notFlaggedHiddenBombCells board
                           : [flaggedHiddenValueCells board x | x <- [0 .. 8]]
                           ++ [notFlaggedHiddenValueCells board x | x <- [0 .. 8]]
                           ++ [notFlaggedNotHiddenValueCells board x | x <- [0 .. 8]]
             
--chooses whether to get the running or gameover picture based on the games state and then also translates the entire board to the centre of the screen
gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where
        frame = case gameState game of
                    Running -> runningBoardPicture (gameBoard game)
                    GameOver result -> gameoverBoardPicture (gameBoard game) result

