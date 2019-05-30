module Methods where

import Types

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