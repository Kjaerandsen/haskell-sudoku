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
        --testSolve
        testValidateBoard
        testValidateBoardState
        testSolveHorizontal
        testSolveVertical
        testSolveCubic

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


testSolveVertical :: Spec
testSolveVertical = do
        describe "\nTests for the vertical solve subfunction" $ do
            it "Basic puzzle one" $ do
                solveVertical [0,0,0,7,6,0,4,0,0,
                               2,0,7,0,0,0,5,1,8,
                               1,0,0,5,2,0,3,0,0,
                               6,8,0,1,0,9,0,4,0,
                               5,4,0,8,0,2,0,6,9,
                               0,7,0,3,0,6,0,2,5,
                               0,0,6,0,1,5,0,0,7,
                               4,3,5,0,0,0,2,0,1,
                               0,0,9,0,8,3,0,0,0] 
                               `shouldBe` 
                               [[3,7,8,9],
                               [1,2,5,6,9],
                               [1,2,3,4,8],
                               [2,4,6,9],
                               [3,4,5,7,9],
                               [1,4,7,8],
                               [1,6,7,8,9],
                               [3,5,7,8,9],
                               [2,3,4,6]]


testSolveHorizontal :: Spec
testSolveHorizontal = do
        describe "\nTests for the horizontal solve subfunction" $ do
            it "Basic puzzle one" $ do
                solveHorizontal [0,0,0,7,6,0,4,0,0,
                                 2,0,7,0,0,0,5,1,8,
                                 1,0,0,5,2,0,3,0,0,
                                 6,8,0,1,0,9,0,4,0,
                                 5,4,0,8,0,2,0,6,9,
                                 0,7,0,3,0,6,0,2,5,
                                 0,0,6,0,1,5,0,0,7,
                                 4,3,5,0,0,0,2,0,1,
                                 0,0,9,0,8,3,0,0,0] 
                                 ([]::[[Int]]) 
                                 `shouldBe` 
                                 [[1,2,3,5,8,9],
                                 [3,4,6,9],
                                 [4,6,7,8,9],
                                 [2,3,5,7],
                                 [1,3,7],
                                 [1,4,8,9],
                                 [2,3,4,8,9],
                                 [6,7,8,9],
                                 [1,2,4,5,6,7]]

testSolveCubic :: Spec
testSolveCubic = do
        describe "\nTests for the cubic solve subfunction" $ do
            it "Basic puzzle one" $ do
                solveCubic [0,0,0,7,6,0,4,0,0,
                            2,0,7,0,0,0,5,1,8,
                            1,0,0,5,2,0,3,0,0,
                            6,8,0,1,0,9,0,4,0,
                            5,4,0,8,0,2,0,6,9,
                            0,7,0,3,0,6,0,2,5,
                            0,0,6,0,1,5,0,0,7,
                            4,3,5,0,0,0,2,0,1,
                            0,0,9,0,8,3,0,0,0] 
                            `shouldBe` 
                            [[3,4,5,6,8,9],
                            [1,3,4,8,9],
                            [2,6,7,9],
                            [1,2,3,9],
                            [4,5,7],
                            [1,3,7,8],
                            [1,2,7,8],
                            [2,4,6,7,9],
                            [3,4,5,6,8,9]]