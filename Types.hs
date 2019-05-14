module Types where

import Data.List.Split

--Blank means that space is not filled, NoColor means that in that space there will remain empty
data Color = Blank | NoColor | Black | Red deriving (Eq, Show)

--BorderSlice contains either one row or one column of the board
type BoardSlice = [Color]

--Hint says how many and what color boxes to put in a certain row/column
type Hint = (Int, Color, Bool)

--Hints contain vertical or horizontal hints for the puzzle
type Hints = [[Hint]]

--Board rowsNo and columnsNo define puzzle's size
--grid is a puzzle itself, have to be filled with proper colors or left empty
data Board = Board {
        rowsNo    :: Int,
        columnsNo :: Int,
        grid      :: [Color] 
        } deriving (Show)

--createNewBoard generates empty board with a given size
createNewBoard :: Int -> Int -> Board
createNewBoard rNo cNo | rNo>0 && cNo>0 = Board rNo cNo (replicate (rNo*cNo) Blank)
                       | otherwise      = Board 0 0 []

--divideIntoRows divides board grid list into convenient sublists of rows
divideIntoRows :: Board -> [BoardSlice]
divideIntoRows (Board _ c grid) = chunksOf c grid

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
solve :: Board -> Hints -> Hints -> Board
solve b v h | isSolved b = b
            | otherwise  = solve (performSolverLoop b v h) v h

--isSolved checks is puzzle is solved 
isSolved :: Board -> Bool
isSolved (Board _ _ g) = isDone g
       where 
               isDone :: [Color] -> Bool
               isDone [] = True
               isDone (g:gx) | g == Blank = False
                             | otherwise = isDone gx 

--performSolverLoop TODO
performSolverLoop :: Board -> Hints -> Hints -> Board
performSolverLoop b v h = b
