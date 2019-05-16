module Parser where
import Types
import Data.List.Split

parse :: [Char] -> Board
parse xs = parseMainComponents xs

parseMainComponents :: [Char] -> Board
parseMainComponents xs = Board numOfRows numOfCols grid colHints rowHints
    where grid = replicate (numOfCols * numOfRows) Blank
          colHints = parseRows $ rawComponents !! 2
          rowHints = parseRows $ rawComponents !! 1
          (numOfRows, numOfCols) = parseDimensions $ rawComponents !! 0
          numOfComponents = 3
          rawComponents = splitOn "\n\n" xs

parseDimensions :: String -> (Int, Int)
parseDimensions xs = if length(dimensions) == 2
                        then (head dimensions, last dimensions)
                        else error $ "Wrong dimensions: " ++ show dimensions
    where dimensions = map read $ (splitOn " " xs) :: [Int]

parseRows :: String -> Hints
parseRows xs = map parseHintLine rawHintLines
    where rawHintLines = map words (lines xs)

parseCols :: String -> Hints
parseCols xs = parseRows xs

parseHintLine :: [String] -> [Hint]
parseHintLine xs = map parseHint xs 

parseHint :: String -> Hint
parseHint xs = (howMany, color $ last xs, False)
    where howMany = read $ init xs :: Int
          color x | x == 'B'  = Black
                  | x == 'R'  = Red  
                  | x == 'G'  = Green
                  | x == 'Y'  = Yellow
                  | x == 'M'  = Magenta
                  | x == 'C'  = Cyan
                  | x == 'W'  = White
                  | x == 'N'  = Blue 
                  | otherwise = error $ "Wrong color code: " ++ show x 

