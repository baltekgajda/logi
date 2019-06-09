module Color where

import System.Console.ANSI
import Types

--putCharWithColor puts Char with Color xd
putStringWithColor :: String -> System.Console.ANSI.Color -> IO ()
putStringWithColor s c = do
        setSGR [SetColor Background Vivid c]
        putString s
        setSGR [Reset]

putString :: String -> IO ()
putString [] = do
        return ()
putString (c:cs) = do
        putChar c
        putString cs

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
putCellWithColor col | col == Types.Blank     = putString "  "
                     | col == Types.NoColor   = putString "XX"
                     | col == Types.Black     = putStringWithColor "  " System.Console.ANSI.Black
                     | col == Types.Red       = putStringWithColor "  " System.Console.ANSI.Red
                     | col == Types.Green     = putStringWithColor "  " System.Console.ANSI.Green
                     | col == Types.Yellow    = putStringWithColor "  " System.Console.ANSI.Yellow
                     | col == Types.Cyan      = putStringWithColor "  " System.Console.ANSI.Cyan
                     | col == Types.Magenta   = putStringWithColor "  " System.Console.ANSI.Magenta
                     | col == Types.Blue      = putStringWithColor "  " System.Console.ANSI.Blue
