module SolverSpec where 
    
import Test.Hspec
import Solver
import Types

main :: IO ()
main = hspec $ do
    describe "toAdvBoardSlices" $ do
        it "transforms color array to (Color, Bool) array" $ do
            let boardLine = [Blank, Blank]
                hints = [(2, Red, False)]
                input = [(boardLine, hints)]
                expectedBoardLine = [(Blank, False), (Blank, False)]
                expectedOutput = [(expectedBoardLine, hints)]
                actualOutput = toAdvBoardSlices input
            actualOutput `shouldBe` expectedOutput
        it "transforms color array to (Color, Bool) array while respecting hints - ver #1" $ do
            let boardLine = [Red, Red, Blank, Blank]
                hints = [(2, Red, True), (2, Blue, False)]
                input = [(boardLine, hints)]
                expectedBoardLine = [(Red, True), (Red, True), (Blank, False), (Blank, False)]
                expectedOutput = [(expectedBoardLine, hints)]
                actualOutput = toAdvBoardSlices input
            actualOutput `shouldBe` expectedOutput
        it "transforms color array to (Color, Bool) array while respecting hints - ver #2" $ do
            let boardLine = [Blue, Blank, Red, Red, Blank, Blank]
                hints = [(1, Blue, True), (2, Red, True), (2, Blue, False)]
                input = [(boardLine, hints)]
                expectedBoardLine = [(Blue, True), (Blank, False), (Red, True), (Red, True), (Blank, False), (Blank, False)]
                expectedOutput = [(expectedBoardLine, hints)]
                actualOutput = toAdvBoardSlices input
            actualOutput `shouldBe` expectedOutput
    
    describe "flagFilledFields" $ do
        it "returns unmodified AdvBoardSlice and False if fields ambiguously placed" $ do
            let boardLine = [(Blank, False), (Blank, False), (Red, False), (Blank, False), (Blank, False)]
                hints = [(1, Red, False), (1, Red, False)]
                input = (boardLine, hints)
                expectedBoardLine = [(Blank, False), (Blank, False), (Red, False), (Blank, False), (Blank, False)]
                expectedHints  = [(1, Red, False), (1, Red, False)]
                expectedBoolVal = False
                expectedOutput = ((expectedBoardLine, expectedHints), expectedBoolVal)
                actualOutput = flagFilledFields input
            actualOutput `shouldBe` expectedOutput

        it "returns modified AdvBoardSlice and True if fields unambiguously placed - case 1" $ do
            let boardLine = [(Red, False), (Blank, False), (Red, False)]
                hints = [(1, Red, False), (1, Red, False)]
                input = (boardLine, hints)
                expectedBoardLine = [(Red, True), (NoColor, True), (Red, True)]
                expectedHints  = [(1, Red, True), (1, Red, True)]
                expectedBoolVal = True
                expectedOutput = ((expectedBoardLine, expectedHints), expectedBoolVal)
                actualOutput = flagFilledFields input
            actualOutput `shouldBe` expectedOutput


        it "returns modified AdvBoardSlice and True if fields unambiguously placed - case 2" $ do
            let boardLine = [(Red, False), (Black, False), (Blank, False)]
                hints = [(1, Red, False), (1, Black, False)]
                input = (boardLine, hints)
                expectedBoardLine = [(Red, True), (Black, True), (NoColor, True)] -- TODO: Is it proper to mark last field with NoColor ?
                expectedHints  = [(1, Red, True), (1, Black, True)]
                expectedBoolVal = True
                expectedOutput = ((expectedBoardLine, expectedHints), expectedBoolVal)
                actualOutput = flagFilledFields input
            actualOutput `shouldBe` expectedOutput
