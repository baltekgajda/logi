module Color where

import System.Console.ANSI

palette :: [Color]
palette = [Black, Red, Green, Yellow, Blue, Magenta, Cyan, White]

putCharWithColor :: Char -> Color -> IO ()
putCharWithColor x c = do
        setSGR [SetColor Background Vivid c]
        putChar x
        setSGR [Reset]

test :: [Color] -> IO ()
test [] = putChar '\n'
test (c:cs) = do putCharWithColor ' ' c 
                 test cs