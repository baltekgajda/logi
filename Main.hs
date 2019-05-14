module Main where

import qualified Parser
import qualified Solver
import qualified Color

main = do
    rawRiddle <- readFile "./data/dumb.txt"
    riddle <- Parser.parse rawRiddle
    solution <- Solver.solve riddle
    putStrLn solution
    
