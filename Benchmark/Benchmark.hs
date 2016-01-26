import Criterion.Main
import qualified Parser.AigerTools as Parser
import Model.Model
import Minisat.Minisat
import System.Directory
import IC3
import Control.Monad

main :: IO ()
main =
  do
    general <- mapM makeIC3Bench
                 [ "simple1.aag", "simple2.aag", "simple3.aag"
                 , "simple4.aag", "simple5.aag", "simple6.aag"
                 , "simple7.aag", "simpler_counters.aig"
                 , "simple_counters.aig" ]
    counters <- mapM makeIC3Bench
                  [ "counters2.aig", "counters2_neg.aig"
                  , "counters3.aig", "counters3_neg.aig" ]
    defaultMain (bgroup "counters" counters:general)

makeIC3Bench str =
  do
    aigModel <- Parser.getModelFromFile ("examples/" ++ str)
    let model = toModel aigModel
    return (bench str $ nfIO (print $ prove model (safe model)))
