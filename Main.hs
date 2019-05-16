module Main where

import Parser (parse)
import qualified Solver
import Types

main = do
    file <- getContents
    let board = parse file
    print $ show board
    
