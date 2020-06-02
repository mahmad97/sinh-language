module Main where

import Declare
import Parser
import Interp
import TypeCheck

import Prelude hiding (LT, GT, EQ)
import Data.Maybe (fromJust)

import System.Exit
import System.Environment (getArgs)
import Control.Monad.State.Strict
import Data.List (isPrefixOf)

import System.Console.Repline

-- Types

type Repl a = HaskelineT IO a

-- hoistErr :: Show e => Either e a -> Repl a
-- hoistErr (Left str) = do
--   liftIO $ putStrLn (show str)
--   abort
-- hoistErr (Right val) = return val


-- Execution
exec :: String -> Repl ()
exec source = do

  let ast = parseExpr source

  case checkProgram ast of
    Left msg -> liftIO $ putStrLn (show msg)
    Right _ -> liftIO . putStrLn $ show (execute $ ast)

exec2 :: [FilePath] -> IO ()
exec2 source = do
  
  src <- readFile . head $ source
  let ast = parseExpr src

  case checkProgram ast of
    Left msg -> error msg
    Right _ -> print . execute $ ast

-- Commands

-- :load command
load :: [String] -> Repl ()
load args = do
  contents <- liftIO $ readFile (unwords args)
  exec contents


-- :quit command
quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess


-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [(":load"  , fileCompleter)]

-- Default tab completer
comp :: Monad m => WordCompleter m
comp n = do
  let cmds = [":load", ":quit"]
  return $ filter (isPrefixOf n) cmds


options :: [(String, [String] -> Repl ())]
options = [("load", load) , ("quit", quit)]


-- Entry point

completer :: CompleterStyle IO
completer = Prefix (wordCompleter comp) defaultMatcher

shell :: Repl a -> IO ()
shell pre =  evalRepl "SINH> " exec options completer pre


-- Top level

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> shell (return ())
    [fname] -> exec2 $ [fname]
    _ -> putStrLn "invalid arguments"

-- main :: IO ()
-- main = do
--   args <- getArgs
--   src <- readFile . head $ args
--   --print src
--   let ast = parseExpr src
--   --print ast
--   case checkProgram ast of
--     Left msg -> error msg
--     Right _ -> print . execute $ ast
 
