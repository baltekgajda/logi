module Parser where
import Types
import Data.List.Split

parse :: [Char] -> [Char]
parse xs = show $ parseMainComponents xs

parseMainComponents :: [Char] -> [[Char]]
parseMainComponents xs = splitOn "\n\n" xs

parseDimensions :: String -> (Int, Int)
parseDimensions xs = if length(dimensions) == 2
                        then (head dimensions, last dimensions)
                        else error $ "Wrong dimensions: " ++ show dimensions
    where dimensions = map read $ (splitOn " " xs) :: [Int]

parseRows :: String -> [[String]]
parseRows xs = map words (lines xs)

parseCols :: String -> [[String]]
parseCols xs = parseRows xs

parseHint :: String -> Hint
parseHint xs = Hint howMany color False
    where howMany = 1
          color = Red


