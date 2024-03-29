module Main where

import Parser (parse)
import qualified Solver
import Color
import Types

main = do
    file <- getContents
    let board = parse file
    let solution =  Solver.solve board 0 1
    Color.printBoard solution

