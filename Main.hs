module Main where

import qualified Parser
import qualified Solver

main = do
    rawRiddle <- readFile "./data/dumb.txt"
    riddle <- Parser.parse rawRiddle
    solution <- Solver.solve riddle
    putStrLn solution
    
