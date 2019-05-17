module Solver where
import Types

--solve is used to solve a puzzle using given hints (vertical & horizontal)
solve :: Board -> Board
solve b | isSolved b = b
        | otherwise  = solve (performSolverLoop b)

--isSolved checks is puzzle is solved
isSolved :: Board -> Bool
isSolved (Board matrix _ _) = isDone (grid (matrix))
        where
                isDone :: [Color] -> Bool
                isDone [] = True
                isDone (x:xs) | x == Blank = False
                              | otherwise = isDone xs

--performSolverLoop performs one algorithm loop on rows and on columns and returns new board
performSolverLoop :: Board -> Board
performSolverLoop board = rowsBoard
        where
                rowsBoard = solveOneDirection board divideIntoRows boardFromRowSlices
                newBoard = solveOneDirection rowsBoard divideIntoColumns boardFromColumnSlices

-- performs one loop of an algorithm based on two functions given as arguments
-- to loop over rows: divideIntoRows & boardFromRowSlices
-- to loop over columns: divideIntoColumns & boardFromColumnSlices
solveOneDirection :: Board -> (Board -> [BoardSlice]) -> (Board ->[BoardSlice] -> Board) -> Board
solveOneDirection board toBoardSlices boardFromBoardSlices = newBoard
        where
                boardSlices = toBoardSlices board
                functions :: [(BoardSlice -> BoardSlice)]
                --functions to add to the algorithm, first function in the array is applied last
                functions = [f1]
                newBoardSlices = applyFunctions functions boardSlices
                newBoard = boardFromBoardSlices board newBoardSlices

--applyFunctions applies given functions to array of rows in columns
-- IMPORTANT: first function in the array is applied last
applyFunctions :: [(BoardSlice -> BoardSlice)] -> [BoardSlice] -> [BoardSlice]
applyFunctions [] boardSlice = boardSlice
applyFunctions (f:fx) boardSlices = map f (applyFunctions fx boardSlices)

--TODO do usuniecia, tak wyglada przykladowa funkcja pobiera rzad(kolumne) z lamiglowki
-- i wskazowki odpowiadajace temu rzedowi(kolumnie), zwraca zmienione kolowy i hinty
f1 :: BoardSlice -> BoardSlice
f1 (colors, hints) = (newColors, hints)
        where
                len = length colors
                newColors = replicate len Black
