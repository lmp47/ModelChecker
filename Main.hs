{- |
Module : Main
Description : Model check a user-inputted file.
-}

import IC3
import Parser.AigerParser
import Model.Model
import System.Directory
import System.Environment

main :: IO ()
main =
  do
  filepath <- getArgs
  aigModel <- getModelFromFile (head filepath)
  let model = toModel aigModel
  print (prove model (safe model))
