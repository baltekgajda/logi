module Types where

import Data.List.Split

--Blank means that space is not filled, NoColor means that in that space there will remain empty
data Color = Blank | NoColor | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White deriving (Eq, Show)

--Hint says how many and what color boxes to put in a certain row/column
--boolean parameter set to true means that hint was properly used
type Hint = (Int, Color, Bool)

--HintSlice contains hints corresponding to one row or column of the board
type HintSlice = [Hint]

--Hints contain all vertical or horizontal hints for the puzzle
type Hints = [HintSlice]

--BorderSlice contains either one row or one column of the board with corresponding hints
type BoardSlice = ([Color], HintSlice)

--like BoardSlice but contains additional info if field on the board (or several) fulfilled a hint
type AdvBoardSlice = ([(Color, Bool)], HintSlice)

--Matrix is two dimensional array with the size specified with rowsNo & columnsNo
data Matrix = Matrix {
        rowsNo    :: Int,
        columnsNo :: Int,
        grid      :: [Color]
} deriving (Show)

--Board is a data type for a puzzle itself, containing 2d board array
--vertical and horizontal hints direct how to solve the puzzle
data Board = Board {
        matrix  :: Matrix,
        vHints :: Hints,
        hHints :: Hints
        } deriving (Show)

--createNewBoard generates empty board with a given size, also vertical and horizontal hints
createNewBoard :: Int -> Int -> Hints -> Hints -> Board
createNewBoard rNo cNo v h = Board matrix v h
        where matrix | rNo>0 && cNo>0 = Matrix rNo cNo (replicate (rNo*cNo) Blank)
                     | otherwise      = Matrix 0 0 []

--divideIntoRows divides grid list into convenient sublists of rows with corresponding hints
divideIntoRows :: Board -> [BoardSlice]
divideIntoRows (Board matrix vHints _) = zip rows vHints
        where
                cNo = columnsNo (matrix)
                rows = chunksOf cNo (grid (matrix))

--boardFromRowSlices creates new board with updated grid and hints based on row's boardSlices and an old board
boardFromRowSlices :: Board ->[BoardSlice] -> Board
boardFromRowSlices oldBoard slices = Board newMatrix newVHints (hHints oldBoard)
        where
                (rows, newVHints) = unzip slices
                newGrid = concat rows
                oldMatrix = matrix oldBoard
                newMatrix = Matrix (rowsNo oldMatrix) (columnsNo oldMatrix) newGrid

--divideIntoColumns divides board grid list into convenient sublists of columns with corresponding hints
divideIntoColumns :: Board -> [BoardSlice]
divideIntoColumns (Board matrix _ hHints) = zip columns hHints
        where
                cNo = columnsNo (matrix)
                rows = chunksOf cNo (grid (matrix))
                columns = switchBetweenRowsAndColumns rows

--boardFromColumnSlices creates new board with updated grid and hints based on column's boardSlices and an old board
boardFromColumnSlices :: Board ->[BoardSlice] -> Board
boardFromColumnSlices oldBoard slices = Board newMatrix (vHints oldBoard) newHHints
        where
                (columns, newHHints) = unzip slices
                rows = switchBetweenRowsAndColumns columns
                newGrid = concat rows
                oldMatrix = matrix oldBoard
                newMatrix = Matrix (rowsNo oldMatrix) (columnsNo oldMatrix) newGrid

--switchBetweenRowsAndColumns produces array of rows based on array of columns and vice versa
switchBetweenRowsAndColumns :: [[Color]] -> [[Color]]
switchBetweenRowsAndColumns []     = []
switchBetweenRowsAndColumns (r:rs) = ownZip r (switchBetweenRowsAndColumns rs)
        where
                ownZip :: [Color] -> [[Color]] -> [[Color]]
                ownZip (a:as) (b:bs) = (a : b) : (ownZip as bs)
                ownZip a []          = map (\x -> [x]) a


data Tree a = Empty | Node a (Tree a) (Tree a)

isHeap :: Tree Int -> Bool
isHeap Empty = True
isHeap (Node v left right) | isHeap left && isHeap right = isLeftOK && isRightOK
                           | otherwise = False
        where
                isLeftOK = if isEmpty left then True
                           else (getValue left) <= v
                isRightOK = if isEmpty right then True
                                           else (getValue right) <= v


                isEmpty Empty = True
                isEmpty _ = False

                getValue (Node a _ _) = a

t :: Tree Int
t = Node 5 (Node 3 Empty Empty) (Node 0 Empty Empty)

a = do
        x <- getChar
        y <- getChar
        return y