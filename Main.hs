module Main where

import Parser (parse)
import qualified Solver
import Color
import Types

main = do
    file <- getContents
    let board = parse file
    let solution =  Solver.solve board 1 1000
    print $ show solution
    Color.printBoard solution
