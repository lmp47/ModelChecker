-- A tester for the IC3 Algorithm on examples in the
-- examples/ directory

import qualified Parser.AigerParser as This
import qualified Parser.AigerTools as Tool
import qualified Parser.AigModel as Aig
import Model.Model
import System.Environment
import System.Directory
import Data.List
import IC3

main :: IO ()
main = do
       directory <- getArgs
       if (length directory /= 1)
         then error "Requires single directory as argument"
         else do
           let dir = head directory ++ "/"
           files <- getDirectoryContents dir
           tester $ map (dir ++) (filter valid files)

valid :: FilePath -> Bool
valid path =
  let ext = drop (length path - 4) path in
    ext == ".aig" || ext == ".aag"

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
  aigModel <- This.getModelFromFile $ filepath
  print $  "Running algorithm on " ++ filepath
  let model = toModel aigModel
  print $ "Safety property: " ++ show (safe $ model)
  print $ show $ prove model (safe $ model)
