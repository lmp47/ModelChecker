module Test.Tests ( tests ) where

import Test.HUnit
import qualified Parser.AigerParser as Parser
import qualified Parser.AigerTools as Tools
import Parser.AigModel (numVars, numInputs, latches, ands, outputs)
import Model.Model
import Minisat.Minisat
import System.Directory
import Data.List
import Control.Monad
import IC3
import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as HUnit
import Debug.Trace

tests :: IO [C.Test]
tests = do
        pTest <- parseTests
        return $ map (uncurry HUnit.test) [ ("IC3 algorithm tests", ic3Test)
                                          , ("IC3 initiation tests", initiationTest)
                                          , ("IC3 consecution tests", consecutionTest)
                                          , ("IC3 push tests", pushTest)
                                          , ("Parser test", pTest)
                                          , ("Minisat test", minisatTest) ]

-- IC3 Tests
ic3Test :: Test
ic3Test = TestList ( map checkTest
  [ ("simple1.aag", False), ("simple2.aag", True), ("simple3.aag", True)
  , ("simple4.aag", True), ("simple5.aag", False), ("simple6.aag", False)
  , ("simple7.aag", False), ("simpler_counters.aig", True)
  , ("simple_counters.aig", True), ("counters2.aig", True)
  , ("counters2_neg.aig", False), ("counters3.aig", True)
  , ("counters3_neg.aig", False) ] )

checkTest :: ([Char], Bool) -> Test
checkTest (name, bool) =
  TestCase ( do
             aigModel <- Parser.getModelFromFile ("examples/" ++ name)
             let model = toModel aigModel
             assertEqual name bool (prove model (safe model)) )

-- IC3 Initiation Tests
initiationTest :: Test
initiationTest = TestList (map (TestCase.(uncurry assertBool))
                   [ ("initiation1", initiation1), ("initiation2", initiation2)
                   , ("initiation3", initiation3), ("initiation4", initiation4) ])
  where
  initiation1 = not $ initiation (getFrame 2 [[Neg 0, Neg' 0], [Var 0, Var' 0]]) [Var 0]
  initiation2 = initiation (getFrame 2 [[Neg 0, Neg' 0], [Var 0, Var' 0]]) [Var 0, Neg 0]
  initiation3 = not $ initiation (getFrame 6 [[Neg 0, Var 1], [Neg 0, Var 2], [Neg 1, Neg 2, Var 0], [Var 1]]) [Neg 0]
  initiation4 = initiation (getFrame 6 [[Neg 0, Var 1], [Neg 0, Var 2], [Neg 1, Neg 2, Var 0], [Var 0]]) [Var 1]

-- IC3 Consecution Tests
consecutionTest :: Test
consecutionTest = TestList (map (TestCase.(uncurry assertBool))
                    [ ("consecution1", consecution1), ("consecution2", consecution2)
                    , ("consecution3", consecution3), ("consecution4", consecution4)
                    , ("consecution5", consecution5) ])
  where
  consecution1 = not $ consecution (getFrame 2 [[Neg 0, Neg' 0], [Var 0, Var' 0], [Var 0]]) [Var 0]
  consecution2 = not $ consecution (getFrame 2 [[Neg 0, Neg' 0], [Var 0, Var' 0]]) [Var 0]
  consecution3 = consecution (getFrame 2 [[Neg 0, Neg' 0], [Var 0, Var' 0]]) [Var 0, Neg 0]
  consecution4 = consecution (getFrame 6 [ [Neg 0, Var 1], [Neg 0, Var 2]
                                         , [Neg 1, Neg 2, Var 0]
                                         , [Neg' 0, Var' 1], [Neg' 0, Var' 2]
                                         , [Neg' 1, Neg' 2, Var' 0]
                                         , [Var 0, Neg' 0], [Neg 0, Var' 0]
                                         , [Var 1], [Var 2] ]) [Var 1]
  consecution5 = consecution (getFrame 6 [ [Neg 0, Var 1], [Neg 0, Var 2]
                                         , [Neg 1, Neg 2, Var 0]
                                         , [Neg' 0, Var' 1], [Neg' 0, Var' 2]
                                         , [Neg' 1, Neg' 2, Var' 0]
                                         , [Var 0, Neg' 0], [Neg 0, Var' 0]
                                         , [Var 0] ]) [Var 1]
  
-- IC3 Push Tests
pushTest :: Test
pushTest = TestList (map (TestCase.(uncurry assertBool))
             [ ("pushTest1", pushTest1), ("pushTest2", pushTest2)
             , ("pushTest3", pushTest3), ("pushTest4", pushTest4) ])
  where
  model1 = Model { vars = 2
                 , initial = []
                 , transition = [[Neg 0, Neg' 0], [Var 0, Var' 0]]
                 , safe = Neg 0 }
  model2 = Model { vars = 6
                 , initial = []
                 , transition = [ [Neg 0, Var 1], [Neg 0, Var 2]
                                , [Neg 1, Neg 2, Var 0]
                                , [Neg' 0, Var' 1], [Neg' 0, Var' 2]
                                , [Neg' 1, Neg' 2, Var' 0]
                                , [Neg 0, Var' 0], [Var 0, Neg' 0] ]
                 , safe = Neg 0 }
  pushTest1 = not $ snd' $ push (getFrameWith [[Neg 0]] model1) model1 (getFrame 2 [])
  pushTest2 = snd' $ push (getFrameWith [[Neg 0, Var 0]] model1) model1 (getFrame 2 [])
  pushTest3 = snd' $ push (getFrameWith [[Var 0]] model2)  model2 (getFrame 6 [])
  pushTest4 = snd' $ push (getFrameWith [[Neg 0], [Neg 1, Neg 2]] model2) model2 (getFrame 6 [])
  snd' (_, s, _) = s

-- Parse tests
parseTests :: IO (Test)
parseTests = (liftM TestList)
               ( foldl1 (liftM2 (++))
                 ( map parseTest
                   [ "simple1.aag", "simple2.aag", "simple3.aag", "simple4.aag"
                   , "simple5.aag", "simple6.aag", "simple7.aag"
                   , "simpler_counters.aig", "simple_counters.aig", "counters2.aig"
                   , "counters2_neg.aig", "counters3.aig", "counters3_neg.aig" ] ))

parseTest :: String -> IO [Test]
parseTest name =
  do
    aigModel  <- Parser.getModelFromFile ("examples/" ++ name)
    aigModel' <- Tools.getModelFromFile ("examples/" ++ name)
    return (map TestCase
             [ assertEqual (name ++ " numVars") (numVars aigModel) (numVars aigModel')
             , assertEqual (name ++ " numInputs") (numInputs aigModel) (numInputs aigModel')
             , assertEqual (name ++ " latches") (sort (latches aigModel)) (sort (latches aigModel'))
             , assertEqual (name ++ " outputs") (sort (outputs aigModel)) (sort (outputs aigModel'))
             , assertEqual (name ++ " ands") (sort (ands aigModel)) (sort (ands aigModel')) ])

-- Minisat tests
minisatTest :: Test
minisatTest = TestList (map TestCase 
                [ assertEqual "solver1" 0 (getVarValue solver1 [Neg 0] 1)
                , assertEqual "solver1" False (satisfiable $ solveWithAssumps solver1 [Neg 0, Neg' 0])
                , assertEqual "solver2" 0 (getVarValue solver2 [Var 1, Var 2] 0) ] )
  where
  solver1 = (addClauses (addVars newSolver 2) [[Neg 0, Neg' 0], [Var 0, Var' 0]])
  solver2 = (addClauses (addVars newSolver 6) [[Neg 0, Var 1], [Neg 0, Var 2], [Neg 1, Neg 2, Var 0]])
