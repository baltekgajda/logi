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

-- toAdvBoardSlicesTest1 :: IO ()
-- toAdvBoardSlicesTest1 = print advBoardSlices
--     where 
--         boardSlices = [
--                 ([Blank], [(1, Red, False)]),
--                 ([Blank], [(1, Blue, False)])
--             ]
--         advBoardSlices = toAdvBoardSlices boardSlices