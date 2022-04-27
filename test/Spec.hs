-- Testing dependencies
import Test.DocTest(doctest) 
import Test.QuickCheck(quickCheck) 
import Test.Hspec(Spec, hspec, describe, shouldBe, it) 
import Test.Hspec.QuickCheck(prop) 

-- Functions to test
import Lib
import Solver
import GameLogic

main :: IO ()
main = do
    putStrLn "Doctests:"
    doctest ["-isrc", "app/Main.hs"]
    putStrLn "\nHspec tests:"
    hspec $ do
        testSolve

testSolve :: Spec
testSolve =
    describe "\nTests for the sudoku board auto solver" $ do
        it "Basic puzzle one" $ do
            solve "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___" `shouldBe` "358761492267934518194528376682159743543872169971346825826415937435697281719283654"
        it "Basic puzzle two" $ do
            solve "_5__19__4_3842_____97863___9__5_73_63_4___5_15_26_1__8___17469_____9815_7__35__8_" `shouldBe` "256719834138425769497863215981547326364982571572631948825174693643298157719356482"
        it "Invalid input, empty output" $ do
            solve "" `shouldBe` ""
        it "invalid length, empty output" $ do
            solve "_______" `shouldBe` ""
        it "invalid character, empty output" $ do
            solve "____x___" `shouldBe` ""

testValidateBoard :: Spec
testValidateBoard = do    
    describe "\nTests for the sudoku board auto solver" $ do
        it "valid board" $ do
            validateBoard "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___" `shouldBe` True
        it "invalid length" $ do
            validateBoard "" `shouldBe` False
        it "invalid character" $ do
            validateBoard "____x___" `shouldBe` False


testValidateBoardState :: Spec
testValidateBoardState = do
    describe "\nTests for the sudoku board auto solver" $ do
        it "Basic puzzle one, valid" $ do
            validateBoardState "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___" `shouldBe` True
        it "conflict vertical" $ do
            validateBoardState "1________1_______________________________________________________________________" `shouldBe` False
        it "conflict horizontal" $ do
            validateBoardState "11_______________________________________________________________________________" `shouldBe` False
        it "conflict grid" $ do
            validateBoardState "1____________1___________________________________________________________________" `shouldBe` False
