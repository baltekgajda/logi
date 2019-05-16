module Types where

import Data.List.Split

--Blank means that space is not filled, NoColor means that in that space there will remain empty
data Color = Blank | NoColor | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White deriving (Eq, Show)

--BorderSlice contains either one row or one column of the board
type BoardSlice = [Color]

--Hint says how many and what color boxes to put in a certain row/column
-- boolean parameter set to true means that hint was properly used
type Hint = (Int, Color, Bool)

--Hints contain vertical or horizontal hints for the puzzle
type Hints = [[Hint]]

--Board rowsNo and columnsNo define puzzle's size
--grid is a puzzle itself, have to be filled with proper colors or left empty
--vertical and horizontal hints direct how to solve the puzzle
data Board = Board {
    rowsNo    :: Int,
    columnsNo :: Int,
    grid      :: [Color],
    vHints    :: Hints,
    hHints    :: Hints
} deriving (Show)

--createNewBoard generates empty board with a given size
createNewBoard :: Int -> Int -> Hints -> Hints -> Board
createNewBoard rNo cNo v h | rNo>0 && cNo>0 = Board rNo cNo (replicate (rNo*cNo) Blank) v h
                           | otherwise      = Board 0 0 [] [] []

--divideIntoRows divides board grid list into convenient sublists of rows
divideIntoRows :: Board -> [BoardSlice]
divideIntoRows (Board _ c grid _ _) = chunksOf c grid

--divideIntoColumns divides board grid list into convenient sublists of columns
divideIntoColumns :: Board -> [BoardSlice]
divideIntoColumns board = getColumns rows
        where
                rows = divideIntoRows board

                getColumns :: [BoardSlice] -> [BoardSlice]
                getColumns []     = []
                getColumns (r:rs) = ownZip r (getColumns rs)

                ownZip :: BoardSlice -> [BoardSlice] -> [BoardSlice]
                ownZip (a:as) (b:bs) = (a : b) : (ownZip as bs)
                ownZip a []          = map (\x -> [x]) a

--solve is used to solve a puzzle using given hints (vertical & horizontal)
solve :: Board -> Board
solve b | isSolved b = b
        | otherwise  = solve (performSolverLoop b)

--isSolved checks is puzzle is solved 
isSolved :: Board -> Bool
isSolved (Board _ _ g _ _) = isDone g
        where 
                isDone :: [Color] -> Bool
                isDone [] = True
                isDone (x:xs) | x == Blank = False
                              | otherwise = isDone xs

--performSolverLoop TODO
performSolverLoop :: Board -> Board
performSolverLoop b = b
        where
                rows = divideIntoRows b
                afterRowsBoard = solveOneDirection rows (vHints b)
                columns = divideIntoColumns afterRowsBoard
                newBoard = solveOneDirection columns (hHints b)

solveOneDirection :: [BoardSlice] -> Hints -> Board
solveOneDirection (b:bs) (h:hs) = createNewBoard 2 2 [] [] 
