module Methods where

import Types
import Color
import Data.List.Split

--simpleBoxes performs force/simpleBoxes method, which it tries to fit all the hints in the very beginning
--and very end of a slice then these two are compared and if two fields are the same it means that this color
--must be in this place, the rest of the fields is left untouched
simpleBoxes :: AdvBoardSlice -> AdvBoardSlice
simpleBoxes (colorArray, hintSlice) = (newColorArray, hintSlice)
        where
                --fromBeginRev is reversed, fromEnd is not because argument array was reversed
                (_, fromBeginRev, _) = markFirstsReversed colorArray [] hintSlice True 0
                (_, fromEnd, _) = markFirstsReversed (reverse colorArray) [] (reverse hintSlice) True 0
                fromBegin = reverse fromBeginRev
                newColorArray = map (\(a,b) -> decideColor a b) $ zip fromBegin fromEnd
                --find matching fields/ simple boxes matching
                decideColor :: (Color,Bool) -> (Color,Bool) -> (Color,Bool)
                decideColor (c1,b1) (c2,_) | c1 == c2 = (c1, b1)
                                            | otherwise = (Blank, False)

--method used in simpleBoxes, recursively tries to fit hints in the very beginning of the slice,
--arguments: initial slice we want to fill, result slice (empty at the beginning), hintSlice, boolean value
--distinguishing if we start putting colors of the new hint, position in the initial slice
markFirstsReversed :: [(Color,Bool)] -> [(Color,Bool)] -> HintSlice -> Bool -> Int -> (Bool,[(Color,Bool)],Int)
markFirstsReversed (x:xs) result (h:hs) newHint pos = if xColor == Blank || xColor == hintColor
                                            then if isOK
                                                        then (isOK, newList, newPos)
                                                        else if newHint
                                                                    then markFirstsReversed splitB ((reverse splitA)++(x:result))
                                                                            (h:hs) newHint (newPos+1)
                                                                    else (False, [], newPos)
                                            else if newHint
                                                        then markFirstsReversed xs (x:result) (h:hs) newHint (pos+1)
                                                        else (False, [], pos)

        where
                xColor = fst x
                (hintCount, hintColor, _) = h
                hsLength = length hs
                (_,sndHintColor,_) = head hs
                (isOK, newList, newPos) = if hintCount == 1 && hsLength > 0 && sndHintColor == hintColor
                                                  then markFirstsReversed (tail xs) ((head xs):(hintColor,False):result) hs True (pos+2)
                                                  else if hintCount == 1 && hsLength > 0
                                                          then markFirstsReversed xs ((hintColor,False):result) hs True (pos+1)
                                                          else if hintCount == 1
                                                                  then (True, ((reverse xs)++((hintColor,False):result)), (pos+1))
                                                                  else markFirstsReversed xs ((hintColor,False):result)
                                                                          ((hintCount-1,hintColor,False):hs) False (pos+1)
                posDiff = newPos - pos
                (splitA, splitB) = splitAt posDiff xs

--------------------------------------------------------------------------------------------------------------------------------------------



glue :: AdvBoardSlice -> AdvBoardSlice
glue (colorArray, hintSlice) = (newColorArray, hintSlice)
        where
              (colorTempArray1, _) = glueCoreMethod (colorArray, hintSlice)
              (colorTempArray2, _) = glueCoreMethod ((reverse colorTempArray1), (reverse hintSlice))
              newColorArray = reverse colorTempArray2


glueCoreMethod :: AdvBoardSlice -> AdvBoardSlice
glueCoreMethod (colorArray, hintSlice) = (newColorArray, hintSlice)
        where
              arrayLength = length colorArray
              (a, b, c) = getFirstClue hintSlice
              boxPosition = getBoxPosition colorArray 1
              borderPosition = getBorderPosition (reverse colorArray) (a, b, c) arrayLength (arrayLength - boxPosition + 1) 0
              newColorArray = if c == True || boxPosition == 0 || borderPosition == -1
                       then colorArray
                       else getNewColorArray colorArray (a, b, c) boxPosition borderPosition 1


getFirstClue :: [Hint] -> Hint
getFirstClue [x] = x
getFirstClue (x:xs) = if isTrue == False
                        then x
                        else getFirstClue xs
                   where 
                        (_, _, isTrue) = x


getBoxPosition :: [(Color, Bool)] -> Int -> Int
getBoxPosition [x] a = 0
getBoxPosition (x:xs) a = if fst x /= NoColor && fst x /= Blank && snd x == False
                          then a
                          else getBoxPosition xs (a+1)


getBorderPosition :: [(Color,Bool)] -> Hint -> Int -> Int -> Int -> Int
getBorderPosition [x] _ arrayLength _ _ = arrayLength
getBorderPosition (x:xs) (a, b, c) arrayLength boxPosition pos = if (pos+1) >= boxPosition && fst (head xs) /= Blank && fst (head xs) /= fst x
         then if fst (head xs) == b
                  then (arrayLength - pos)
                  else (arrayLength - pos - 1)
         else getBorderPosition xs (a, b, c) arrayLength boxPosition (pos+1)


getNewColorArray :: [(Color, Bool)] -> Hint -> Int -> Int -> Int -> [(Color, Bool)]
getNewColorArray [] _ _ _ _ = []
getNewColorArray (x:xs) (a, b, c) boxPosition borderPosition currentPosition = if currentPosition < boxPosition
                       then x : (getNewColorArray xs (a, b, c) boxPosition borderPosition (currentPosition+1))
                       else if currentPosition < (boxPosition + (a - (boxPosition - borderPosition - 1)))
                            then (b, False) : (getNewColorArray xs (a, b, c) boxPosition borderPosition (currentPosition+1))
                            else x : (getNewColorArray xs (a, b, c) boxPosition borderPosition (currentPosition+1))

--------------------------------------------------------------------------------------------------------------------------------------------


simpleSpaces :: AdvBoardSlice -> AdvBoardSlice
simpleSpaces (colorArray, hintSlice) = (newColorArray, hintSlice)
            where newColorArray | areHintsFilled hintSlice == True || areColorsFilled colorArray hintSlice == True = markEmptyCells colorArray
                                | otherwise = bruteForceSpaces (colorArray, hintSlice)


areHintsFilled :: [Hint] -> Bool
areHintsFilled [] = True
areHintsFilled (x:xs) = third x && areHintsFilled xs

markEmptyCells :: [(Color, Bool)] -> [(Color, Bool)]
markEmptyCells [] = []
markEmptyCells (x:xs) = (newColor, snd x) : markEmptyCells xs
         where newColor | fst x == Blank = NoColor
                        | otherwise = fst x

areColorsFilled :: [(Color, Bool)] -> [Hint] -> Bool
areColorsFilled a b | sumHint b == sumColoredCells a = True
                    | otherwise = False

sumHint :: [Hint] -> Int
sumHint [] = 0
sumHint (x:xs) = first x + sumHint xs

sumColoredCells :: [(Color, Bool)] -> Int
sumColoredCells [] = 0
sumColoredCells (x:xs) | (fst x) /= Blank && (fst x) /= NoColor = 1 + (sumColoredCells xs)
                       | otherwise = 0 + sumColoredCells xs

bruteForceSpaces :: AdvBoardSlice -> [(Color, Bool)]
bruteForceSpaces (colorArray, hintSlice) = createNewColorArray [getArray x | x <- removeAllDups (getAll (colorArray, hintSlice)), (colorArrayIsGood x 1) && (colorArraysFit (getArray x) colorArray)] colorArray 1

colorArrayIsGood :: [[Color]] -> Int -> Bool
colorArrayIsGood [x] _ = True
colorArrayIsGood (x:xs) a = if a `mod` 2 == 1
      then True && colorArrayIsGood xs (a+1)
      else if length xs > 1 
           then if head x == head (head (tail xs)) 
                   then if length (head xs) > 0
                           then True
                           else False
                   else True
           else True

colorArraysFit :: [Color] -> [(Color, Bool)] -> Bool
colorArraysFit [] [] = True
colorArraysFit (x:xs) (y:ys) = if fst y == Blank
     then True && colorArraysFit xs ys
     else if fst y == NoColor
            then if x == Blank
                then True && colorArraysFit xs ys
                else False
            else if fst y == x
                then True && colorArraysFit xs ys
                else False

createNewColorArray :: [[Color]] -> [(Color, Bool)] -> Int -> [(Color, Bool)]
createNewColorArray _ [] _ = []
createNewColorArray allPossiblePermutations (y:ys) pos = if fst y == NoColor
     then [y] ++ createNewColorArray allPossiblePermutations ys (pos+1)
     else if fst y == Blank
          then if areColorsTheSame allPossiblePermutations pos == True
                then if (head allPossiblePermutations)!!(pos-1) == Blank 
                        then [(NoColor, False)] ++ createNewColorArray allPossiblePermutations ys (pos+1)
                        else [((head allPossiblePermutations)!!(pos-1), False)] ++ createNewColorArray allPossiblePermutations ys (pos+1)
                else [(Blank, False)] ++ createNewColorArray allPossiblePermutations ys (pos+1)
          else [y] ++ createNewColorArray allPossiblePermutations ys (pos+1)

getArray :: [[Color]] -> [Color]
getArray [x] = x
getArray (x:xs) = x ++ getArray xs


areColorsTheSame :: Eq a => [[a]] -> Int -> Bool
areColorsTheSame [x] _ = True
areColorsTheSame (x:xs) pos = x!!(pos-1) == (head xs)!!(pos-1) && areColorsTheSame xs pos

getAll :: AdvBoardSlice -> [[[Color]]]
getAll (colors, slices) = [x | x <- fillSoll [(generateBasicSolution slices)] 1 (length (generateBasicSolution slices)) ((length colors) - (sumHint slices)), howManyColors x == length colors]

generateBasicSolution :: [(Int, Color, Bool)] -> [[Color]]
generateBasicSolution [] = [[]]
generateBasicSolution (x:xs) = [[], (replicate (first x) (second x))] ++ generateBasicSolution xs

fillSoll :: [[[Color]]] -> Int -> Int -> Int -> [[[Color]]]
fillSoll [] _ _ _ = []
fillSoll [x] position maxPos maxColors | position == maxPos = (generateSoll x position 0 maxColors)
                                       | otherwise  = newColors ++ fillSoll newColors (position+2) maxPos maxColors
                              where newColors = generateSoll x position 0 maxColors
fillSoll (x:xs) position maxPos maxColors = fillSoll [x] position maxPos maxColors ++ fillSoll xs position maxPos maxColors


generateSoll :: [[Color]] -> Int -> Int -> Int -> [[[Color]]]
generateSoll colors position howManyColors maxColors | howManyColors <= maxColors = [replaceAt colors position (replicate howManyColors Blank)] ++ generateSoll colors position (howManyColors+1) maxColors
                                                     | otherwise = [[[]]]


replaceAt :: [a] -> Int -> a -> [a]
replaceAt colors gdzie co = insertAt (removeAt colors gdzie) co (gdzie-1)

insertAt :: [a] -> a -> Int -> [a]
insertAt lista znak gdzie = take gdzie lista ++ [znak] ++ drop gdzie lista

removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt (x:xs) n | n == 1 = xs
                  | otherwise = x : (removeAt xs (n-1))

removeAllDups :: Eq a => [a] -> [a]
removeAllDups [] = []
removeAllDups [x] = [x]
removeAllDups (x:xs) = if not (x `elem` xs) then [x] ++ removeAllDups(xs) else removeAllDups(xs)

howManyColors :: [[Color]] -> Int
howManyColors [] = 0
howManyColors (x:xs) = length x + howManyColors xs

first :: Hint -> Int
first (a, _, _) = a

second :: Hint -> Color
second (_, b, _) = b

third :: Hint -> Bool
third (_, _, c) = c