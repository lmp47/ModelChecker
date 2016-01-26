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

tests :: IO [C.Test]
tests = do
        pTest <- parseTests
        return $ map (uncurry HUnit.test) [ ("IC3 Algorithm Tests", ic3Test)
                                          , ("Parser Test", pTest)
                                          , ("Minisat Test", minisatTest) ]

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

minisatTest :: Test
minisatTest = TestList (map TestCase 
                [ assertEqual "solver1" 0 (getVarValue solver1 [Neg 0] 1)
                , assertEqual "solver1" False (satisfiable $ solveWithAssumps solver1 [Neg 0, Neg' 0])
                , assertEqual "solver2" 0 (getVarValue solver2 [Var 1, Var 2] 0) ] )
  where
  solver1 = (addClauses (addVars newSolver 2) [[Neg 0, Neg' 0], [Var 0, Var' 0]])
  solver2 = (addClauses (addVars newSolver 6) [[Neg 0, Var 1], [Neg 0, Var 2], [Neg 1, Neg 2, Var 0]])
