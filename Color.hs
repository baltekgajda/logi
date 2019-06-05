module Color where

import System.Console.ANSI
import Types

--putCharWithColor puts Char with Color xd
putCharWithColor :: Char -> System.Console.ANSI.Color -> IO ()
putCharWithColor x c = do
        setSGR [SetColor Background Vivid c]
        putChar x
        putChar x
        setSGR [Reset]

printBoard :: Board -> IO()
printBoard board = printBoardSlice (divideIntoRows board)

printBoardSlice :: [BoardSlice] -> IO()
printBoardSlice [] = putChar '\n'
printBoardSlice (x:xs) = do printRow (fst x)
                            printBoardSlice xs

printRow :: [Types.Color] -> IO ()
printRow [] = putChar '\n'
printRow (x:xs) = do putCellWithColor x
                     printRow xs

putCellWithColor :: Types.Color -> IO ()
putCellWithColor col | col == Types.Blank     = do putChar ' '
                                                   putChar ' '
                     | col == Types.NoColor   = do putChar '>'
                                                   putChar '<'
                     | col == Types.Black     = putCharWithColor ' ' System.Console.ANSI.Black
                     | col == Types.Red       = putCharWithColor ' ' System.Console.ANSI.Red
                     | col == Types.Green     = putCharWithColor ' ' System.Console.ANSI.Green
                     | col == Types.Yellow    = putCharWithColor ' ' System.Console.ANSI.Yellow
                     | col == Types.Cyan      = putCharWithColor ' ' System.Console.ANSI.Cyan
                     | col == Types.Magenta   = putCharWithColor ' ' System.Console.ANSI.Magenta
                     | col == Types.Blue      = putCharWithColor ' ' System.Console.ANSI.Blue