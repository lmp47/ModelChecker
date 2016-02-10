import Criterion.Main
import qualified Parser.AigerTools as Parser
import Model.Model
import Minisat.Minisat
import System.Directory
import System.Environment
import IC3
import Control.Monad

main :: IO ()
main =
  do
    let directories = ["examples"]
    bgroups <- mapM makeBgroup directories
    defaultMain bgroups

makeBgroup :: String -> IO (Benchmark)
makeBgroup directory = 
  do
    files <- getDirectoryContents directory
    benches <- mapM makeIC3Bench (map ((directory ++ "/") ++) (filter valid files))
    return (bgroup directory benches)

valid :: FilePath -> Bool
valid path =
  let ext = drop (length path - 4) path in
    ext == ".aig" || ext == ".aag"

makeIC3Bench filepath =
  do
    aigModel <- Parser.getModelFromFile filepath
    let model = toModel aigModel
    return (bench filepath $ nfIO (print $ prove model (safe model)))
