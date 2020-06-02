module Main where

import Declare
import Parser
import Interp
import TypeCheck

import Prelude hiding (LT, GT, EQ)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  src <- readFile . head $ args
  --print src
  let ast = parseExpr src
  --print ast
  case checkProgram ast of
    Left msg -> error msg
    Right _ -> print . execute $ ast
 
