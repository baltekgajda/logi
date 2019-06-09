module Solver where

import Types
import Methods

import Data.List.Split

--solve is used to solve a puzzle using given hints (vertical & horizontal)
solve :: Board -> Int -> Int -> Board
solve b c limit| isSolved b || c>limit = b
               | otherwise = solve (performSolverLoop b) (c+1) limit

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
solveOneDirection board toSlicesFun boardFromSlicesFun = newBoard
        where
                boardSlices = toSlicesFun board
                advBoardSlices = toAdvBoardSlices boardSlices
                functions :: [(AdvBoardSlice -> AdvBoardSlice)]
                --functions to add to the algorithm
                functions = [simpleBoxes,glue,f3]    --TODO zmienic nazwy
                newAdvBoardSlices = [divideAndApply slice | slice <- advBoardSlices]
                divideAndApply :: AdvBoardSlice -> AdvBoardSlice
                divideAndApply slice = joinBackSubAdvBoardSlices slice afterSlices
                        where
                                subdivided = divideIntoSubAdvBoardSlices slice
                                afterSlices = [applyFunctions functions s | s <- subdivided]

                newBoardSlices = toBoardSlices newAdvBoardSlices
                newBoard = boardFromSlicesFun board newBoardSlices

--applyFunctions applies given functions to array of rows or columns
applyFunctions :: [(AdvBoardSlice -> AdvBoardSlice)] -> AdvBoardSlice -> AdvBoardSlice
applyFunctions [] boardSlice = boardSlice
applyFunctions (f:fx) boardSlice = if toDivide then joinBackSubAdvBoardSlices afterApplied slicesToJoin
                         else applyFunctions fx afterApplied
        where
                afterApplied = f boardSlice
                (flaggedSlice, toDivide) = flagFilledFields afterApplied
                dividedSlices = divideIntoSubAdvBoardSlices flaggedSlice
                slicesToJoin = [applyFunctions (f:fx) s | s <- dividedSlices]

--for each boardSlice all done fields are marked using boolean values in new advBoardSlice
toAdvBoardSlices :: [BoardSlice] -> [AdvBoardSlice]
toAdvBoardSlices boardSlices = newAdvBoardSlices
        where
                getAdvColorArray array = [(c,False) | c <- array]
                advBoardSlices = [(getAdvColorArray color, hints) | (color, hints) <- boardSlices]
                newAdvBoardSlices = [(\(x,y) -> if y then x else abs) (flagFilledFields abs) | abs <- advBoardSlices]

--toBoardSlices transforms advBoardSlices into BoardSlices
toBoardSlices :: [AdvBoardSlice] -> [BoardSlice]
toBoardSlices b = [(getColorArray advColor, hints) | (advColor, hints) <- b]
        where
                getColorArray :: [(Color, Bool)] -> [Color]
                getColorArray array = [c | (c,_) <- array]

--divides boardSlice into boardSlices by removing fulfilled hints, leaving only
--these fields that still have to be filled in, empty list means that slice is done
divideIntoSubAdvBoardSlices :: AdvBoardSlice -> [AdvBoardSlice]
divideIntoSubAdvBoardSlices (colorArray, hints) = zip newColorArrays newHints
        where
                splittedColorArray = splitWhen (\(_,b) -> b) colorArray          --remove done fragments of a slice
                splittedHints = splitWhen (\(_,_,b) -> b) hints                  --and also fulfilled hints
                newColorArrays = filter (\x -> length x > 0) splittedColorArray  --remove empty arrays from array
                newHints = filter (\x -> length x > 0) splittedHints

--joins previously subdivided slices and places it onto the old slice
joinBackSubAdvBoardSlices :: AdvBoardSlice -> [AdvBoardSlice] -> AdvBoardSlice
joinBackSubAdvBoardSlices oldSlice subSlices = (newColorArray, newHints)
        where
                (oldColorArray, oldHints) = oldSlice
                (first, second) = unzip subSlices
                zippedColorArrays = concat first
                zippedHints = concat second

                newColorArray = reverse (joinColorArrays oldColorArray zippedColorArrays [])
                newHints = reverse (joinHints oldHints zippedHints [])

                joinColorArrays [] [] newArray = newArray
                joinColorArrays (x:xs) [] newArray = joinColorArrays xs [] (x:newArray)
                joinColorArrays [] (y:ys) newArray = joinColorArrays [] ys (y:newArray)
                joinColorArrays (x:xs) (y:ys) newArray = if snd x then joinColorArrays xs (y:ys) (x:newArray)
                                                         else joinColorArrays xs ys (y:newArray)

                joinHints [] [] newArray = newArray
                joinHints (x:xs) [] newArray = joinHints xs [] (x:newArray)
                joinHints [] (y:ys) newArray = joinHints [] ys (y:newArray)
                joinHints (x:xs) (y:ys) newArray = if isDone then joinHints xs (y:ys) (x:newArray)
                                                   else joinHints xs ys (y:newArray)
                        where (_,_,isDone) = x

f3 :: AdvBoardSlice -> AdvBoardSlice
f3 slice = slice


board = Board {matrix = Matrix {rowsNo = 2, columnsNo = 2, grid = [Blank,Blank,Blank,Blank]}, vHints = [[(2,Black,False),(1,Red,False)],[(1,Red,False),(2,Black,False)]], hHints = [[(3,Black,False),(2,Red,False)],[(4,Black,False),(2,Red,False)]]}