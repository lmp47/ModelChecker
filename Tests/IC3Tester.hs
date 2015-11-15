-- A tester for the IC3 Algorithm

import qualified Parser.AigerParser as This
import qualified Parser.AigerTools as Tool
import qualified Parser.AigModel as Aig
import Model.Model
import System.Directory
import Data.List
import IC3

main :: IO ()
main = do
       filepaths <- getDirectoryContents "simple/"
       tester $ drop 2 filepaths
       

tester :: [FilePath] -> IO()
tester (f:fps) =
  do
  test f
  tester fps
tester [] =
  print "Done."

test :: FilePath -> IO()
test filepath =
  do
  aigModel <- This.getModelFromFile $ "simple/" ++ filepath
  print $  "Testing algorithm on simple/" ++ filepath
  let model = toModel aigModel
  print $ "Safety property: " ++ show (safe $ model)
  prove model (safe $ model) []
