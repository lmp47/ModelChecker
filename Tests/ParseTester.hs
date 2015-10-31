-- A tester for the Aiger format readers

import qualified Parser.AigerParser as This
import qualified Parser.AigerTools as Tool
import Parser.AigModel
import System.Directory
import Data.List

isPerm :: (Eq a, Ord a) => [a] -> [a] -> Bool
isPerm l1 l2 = ((sort l1) == (sort $ l1 `intersect` l2))

instance Eq Model where
  (==) m1 m2 = (inputs m1) `isPerm` (inputs m2)                   &&
               (latches m1) `isPerm` (latches m2)                 &&
               (outputs m1) `isPerm` (outputs m2)                 &&
               (map sort $ ands m1) `isPerm` (map sort $ ands m2) &&
               (bad m1) `isPerm` (bad m2)                         &&
               (constraints m1) `isPerm` (constraints m2) 

main :: IO ()
main = do
       filepaths <- getDirectoryContents "examples/"
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
  model1 <- This.getModelFromFile $ "examples/" ++ filepath
  model2 <- Tool.getModelFromFile $ "examples/" ++ filepath
  print $  "Testing parser on examples/" ++ filepath
  if (model1 /= model2) then
    do -- todo: more helpful output
    print ("Different Inputs: " ++ show ((inputs model1) \\ (inputs model2)))
    print ("Different Latches: " ++ show ((latches model1) \\ (latches model2)))
    print ("Different Outputs: " ++ show ((outputs model1) \\ (outputs model2)))
    print ("Different Ands: " ++
           show ((map sort $ ands model1) \\ (map sort $ ands model2)))
    print ("Different Bad:" ++ show ((bad model1) \\ (bad model2)))
    print ("Different Constraints: " ++
           show ((constraints model1) \\ (constraints model2)))
    error "Failed test."
  else return ()
