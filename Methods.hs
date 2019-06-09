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

--glue performs force/simpleBoxes method, which it tries to fit all the hints in the very beginning
--and very end of a slice then these two are compared and if two fields are the same it means that this color
--must be in this place, the rest of the fields is left untouched
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
getBorderPosition [x] _ arrayLength _ pos = 0
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

--function that takes color array and returns array that says where different colorful pieces
--where placed (positions) e.g. Black Black Blank Red Green -> [(Black,0),(Red,3),(Green,4)]
--arguments: input array, position in input array, last used color, output array
getFirstsPos :: [(Color,Bool)] -> Int -> Color -> [(Color,Int)]
getFirstsPos [] _ _ = []
getFirstsPos (x:xs) pos last = if color /= Blank && color /= NoColor
                                then if color /= last
                                        then (color,pos):getFirstsPos xs (pos+1) color
                                        else getFirstsPos xs (pos+1) last
                                else getFirstsPos xs (pos+1) Blank
        where
                color = fst x

--function that works analogically to getFirstsPos but returns lasts position of each
--chunk of color e.g. Black Black Blank Red Green -> [(Black,1),(Red,3),(Green,4)]
getLastsPos :: [(Color,Bool)] -> Int -> Color -> [(Color,Int)]
getLastsPos list pos last = map (\(x,y) -> (x,len-1-y)) (reverse lastsFlippedPos)
        where
                lastsFlippedPos = getFirstsPos (reverse list) 0 Blank
                len = length list

--based on two arrays containing information about very first and very last positions possible
--in certain row/column function create section for each hint where on which positions it can be
getHintsSections:: [(Color,Int)] -> [(Color,Int)] -> [(Color,(Int,Int))]
getHintsSections [] [] = []
getHintsSections (x:xs) (y:ys) = (fst x,(snd x,snd y)):(getHintsSections xs ys)

--for array of sections for each hint this function separates them so same colors don't overlap
--for different colors its no problem to overlap so it doesn't separate those
--e.g. [(Black,(2,4)),(Black,(3,5))] -> [(Black,(2,2)),(Black,(5,5))]
makeSelectionsDisjointed :: [(Color,(Int,Int))] -> [(Color,(Int,Int))] -> [(Color,(Int,Int))]
makeSelectionsDisjointed [] _ = []
makeSelectionsDisjointed (x:xs) wholeList= (findOverlap x wholeList):(makeSelectionsDisjointed xs wholeList)
        where
                findOverlap :: (Color,(Int,Int)) -> [(Color,(Int,Int))] -> (Color,(Int,Int))
                findOverlap hint [] = hint
                findOverlap (color1,(beg1,end1)) ((color2,(beg2,end2)):ys) = if color1 /= color2 || beg1 == beg2 || end1 == end2
                                                                                    then findOverlap (color1,(beg1,end1)) ys
                                                                             else findOverlap (color1,(newBeg,newEnd)) ys
                        where
                                newBeg = if end2 >= beg1 && beg2 < beg1 then end2+1 else beg1
                                newEnd = if end1 >= beg2 && beg2 > beg1 then beg2-1 else end1

--function similar to getFirstsPos and getLastsPos, from an array it produces another array
--containing info about where (which position in array) and what color starts and ends
--e.g. Black Black Blank Red Green -> [(Black,(0,1)),(Red,(3,3)),(Green,(4,4))]
--to use function write: getColorsPos array 0 (Blank,0)
getColorsPos :: [(Color,Bool)] -> Int -> (Color,Int) -> [(Color,(Int,Int))]
getColorsPos [] pos (lastColor, begin) = if lastColor == Blank || lastColor == NoColor
                                                then []
                                         else [(lastColor,(begin,pos-1))]
getColorsPos ((color,_):xs) pos (lastColor, begin) = if color == Blank || color == NoColor
                                                            then if lastColor /= Blank && lastColor /= NoColor
                                                                        then (lastColor,(begin,pos-1)):(getColorsPos xs (pos+1) (Blank,pos))
                                                                 else getColorsPos xs (pos+1) (color,pos)
                                                     else if color == lastColor
                                                                 then getColorsPos xs (pos+1) (lastColor,begin)
                                                          else if lastColor /= Blank && lastColor /= NoColor
                                                                      then (lastColor,(begin,pos-1)):(getColorsPos xs (pos+1) (color,pos))
                                                               else getColorsPos xs (pos+1) (color,pos)

--returns an array of the size of input HintSlice, it is a tuple (Hint,Int)
--hint is the same but can be noted as Done, second value indicate starting pos of Done hint
--if done, -1 otherwise; arguments are: array of hints, hintSelectionsDisjointed (returned from makeSelectionsDisjointed)
--and color positions in ColorArray (return of getColorsPos),
findDoneHints :: HintSlice -> [(Color,(Int,Int))] -> [(Color,(Int,Int))] -> [(Hint,Int)]
findDoneHints [] [] _ = []
findDoneHints (x:xs) (y:ys) list = (findDoneHint x y list):(findDoneHints xs ys list)  --TODO change first elem to return list to not to check every elem
        where
                findDoneHint :: Hint -> (Color,(Int,Int)) -> [(Color,(Int,Int))] -> (Hint,Int)
                findDoneHint hint _ [] = (hint,-1)
                findDoneHint hint hintSel (l:ls) = if selectionSize < hintCount
                                                          then (hint,-1)
                                                   else if hintColor == foundColor && fBegin >= hBegin && fEnd <= hEnd && (fBegin+hintCount-1) == fEnd
                                                                then ((hintCount,hintColor,True),fBegin)
                                                        else if fBegin > hEnd
                                                                    then (hint,-1)
                                                             else findDoneHint hint hintSel ls
                        where
                                (hintCount,hintColor,_) = hint
                                (_,(hBegin,hEnd)) = hintSel
                                (foundColor,(fBegin,fEnd)) = l
                                selectionSize = hEnd-hBegin+1

--create mask to use with colorArray to mark all doneFields, in returned array if field is Blank that means
--that mask doesn't apply there, otherwise colorArray fields will be changed to what is in the mask
--first argument is doneHints (returned from findDoneHints), second length of an array, third and fourth are
--last and lastDone hint tuple (Color,wasDone,nexPos) -> nextPos means index after the hint done
--Returns mask and true/false if anything is in the mask. To use: createDoneMask array len (NoColor,True,0) (NoColor,0)
createDoneMask :: [(Hint,Int)] -> Int -> (Color,Bool,Int) -> (Color,Int) -> [(Color,Bool)]
createDoneMask [] len lastHint lastDoneHint = if wasLastDone then replicate (len-lastPos) (NoColor,True)
                                              else replicate (len-lastDonePos) (Blank,False)
        where
                (_,lastDonePos) = lastDoneHint
                (_,wasLastDone,lastPos) = lastHint
createDoneMask (h:hs) len lastHint lastDoneHint = if isHintDone
                                                        then if wasLastDone
                                                                    then (replicate (hintPos-lastPos) (NoColor,True))
                                                                              ++(replicate hintCount (hintColor,True))
                                                                                  ++(createDoneMask hs len (hintColor,True,(hintPos+hintCount)) (hintColor,(hintPos+hintCount)))
                                                             else if hintColor == lastColor
                                                                          then (replicate (hintPos-lastDonePos-1) (Blank,False)) ++ [(NoColor,True)]
                                                                                    ++(replicate hintCount (hintColor,True))
                                                                                           ++(createDoneMask hs len (hintColor,True,(hintPos+hintCount)) (hintColor,(hintPos+hintCount)))
                                                                  else (replicate (hintPos-lastDonePos) (Blank,False))
                                                                           ++(replicate hintCount (hintColor,True))
                                                                                 ++(createDoneMask hs len (hintColor,True,(hintPos+hintCount)) (hintColor,(hintPos+hintCount)))
                                                  else if wasLastDone
                                                              then if hintColor == lastColor
                                                                          then (replicate 1 (NoColor,True))
                                                                                    ++(createDoneMask hs len (hintColor,False,-1) (lastDoneColor,(lastDonePos+1)))
                                                                   else createDoneMask hs len (hintColor,False,-1) lastDoneHint
                                                       else createDoneMask hs len (hintColor,False,-1) lastDoneHint
        where
                ((hintCount,hintColor,isHintDone),hintPos) = h
                (lastDoneColor,lastDonePos) = lastDoneHint
                (lastColor,wasLastDone,lastPos) = lastHint

--applies return array of createDoneMask to first argument which is colorArray, blank field in mask means that value is taken
--from the first argument, from mask otherwise
applyColorDoneMask :: [(Color,Bool)] -> [(Color,Bool)] -> [(Color,Bool)]
applyColorDoneMask [] [] = []
applyColorDoneMask (orig:xs) (mask:ms) = if (fst mask) == Blank then orig:(applyColorDoneMask xs ms)
                                                else mask:(applyColorDoneMask xs ms)

--argument is doneHints (returned from findDoneHints), returns HintSlice and True if some hints were done
getNewHintsDone :: [(Hint,Int)] -> (HintSlice,Bool)
getNewHintsDone hints = (hintArray,foundDone)
        where
                (hintArray,posArray) = unzip hints
                foundDone = if length (filter (\x -> x /= -1) posArray) > 0 then True else False

--checks if all fields are filled, then return true and new array with fields marked as done
checkIfAllDone :: [(Color,Bool)] -> (Bool,[(Color,Bool)])
checkIfAllDone colors = if isDone colors then (True,[(fst c,True) | c <- colors]) else (False,colors)
        where
                isDone [] = True
                isDone (c:cs) | fst c == Blank = False
                              | otherwise = isDone cs

--checks if any new hints were fulfilled, if so flags appropriate fields and hints
--and returns new slice and true value
flagFilledFields :: AdvBoardSlice -> (AdvBoardSlice, Bool)
flagFilledFields slice = if done
                                then ((doneSlice,allDoneHints),True)
                        else if not areAnyNewHintsDone
                                    then (slice,False)
                             else ((newColorArray, newHints),True)
        where
                (colorArray,hints) = slice
                len = length colorArray
                (done,doneSlice) = checkIfAllDone colorArray
                allDoneHints = [(hintCount,hintColor,True) | (hintCount,hintColor,_) <- hints]

                onlyNoColorBlankArray = [(\(x,y) -> if x == NoColor then (x,y) else (Blank,False)) c| c <- colorArray]
                (_, fromBeginRev, _) = markFirstsReversed onlyNoColorBlankArray [] hints True 0
                (_, lasts, _) = markFirstsReversed (reverse onlyNoColorBlankArray) [] (reverse hints) True 0
                firsts = reverse fromBeginRev

                firstsPos = getFirstsPos firsts 0 Blank
                lastsPos = getLastsPos lasts 0 Blank
                firstsLastsPos = getHintsSections firstsPos lastsPos
                selDisjointed = makeSelectionsDisjointed firstsLastsPos firstsLastsPos
                colorsPos = getColorsPos colorArray 0 (Blank,0)
                doneHints = findDoneHints hints selDisjointed colorsPos
                (newHints, areAnyNewHintsDone) = getNewHintsDone doneHints
                colorArrayMask = createDoneMask doneHints len (NoColor,True,0) (NoColor,0)
                newColorArray = applyColorDoneMask colorArray colorArrayMask

