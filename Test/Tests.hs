module Test.Tests ( tests ) where

import Test.HUnit
import qualified Parser.AigerParser as Parser
import Model.Model
import Minisat.Minisat
import System.Directory
import Data.List
import IC3
import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as HUnit

tests :: IO [C.Test]
tests = return $ map (uncurry HUnit.test) [ ("IC3 Algorithm Tests", ic3Test) ]

ic3Test :: Test
ic3Test = TestList ( map simpleTest
  [ ("simple1.aag", False), ("simple2.aag", True), ("simple3.aag", True)
  , ("simple4.aag", True), ("simple5.aag", False), ("simple6.aag", False)
  , ("simple7.aag", False), ("simpler_counters.aig", True)
  , ("simple_counters.aig", True), ("counters2.aig", True)
  , ("counters2_neg.aig", False), ("counters3.aig", True)
  , ("counters3_neg.aig", False) ] )

simpleTest :: ([Char], Bool) -> Test
simpleTest (name, bool) =
  TestCase ( do
             aigModel <- Parser.getModelFromFile ("simple/" ++ name)
             let model = toModel aigModel
             assertEqual name bool (prove model (safe model)) )
