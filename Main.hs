module Main where

import Parser (parse)
import qualified Solver
import Types

main = interact $ parse 
    
