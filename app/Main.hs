module Main where

import Prelude hiding (LT, GT, EQ)
import Declare
import Parser 
import Interp
import TypeCheck

import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)
import System.Exit
import System.Console.Repline
import Control.Monad.State.Strict

type Repl a = HaskelineT IO a

version = "1.0.0"

hoistErr :: String -> Repl ()
hoistErr str = liftIO $ putStrLn $ red ++ str ++ colorReset

hoistErr2 :: String -> IO ()
hoistErr2 str = putStrLn $ red ++ str ++ colorReset


-- Execution
exec :: String -> Repl ()
exec src = do
  let ast = parseExpr src
  case ast of
    (Ok a) -> case checkProgram a of
                Right t -> (do
                  let value = execute a
                  case value of
                    (RaiseV v) -> hoistErr $ show (RaiseV v)
                    _ -> liftIO $ putStrLn $ show value)
                Left err -> hoistErr err

    (Failed e) -> hoistErr e


-- Directly Execute program file without loading the shell
load2 :: String -> IO ()
load2 fname = do
  src <- liftIO $ readFile fname
  let ast = parseExpr src
  case ast of
    (Ok a) -> case checkProgram a of
                Right t -> (do
                  let value = execute a
                  case value of
                    (RaiseV v) -> hoistErr2 $ show (RaiseV v)
                    _ -> putStrLn $ show value)
                Left err -> hoistErr2 err

    (Failed e) -> hoistErr2 e

-- Commands

-- :load command
load :: [String] -> Repl ()
load args = do
  contents <- liftIO $ readFile (unwords args)
  exec contents


-- :quit command
quit :: a -> Repl ()
quit _ = do
  liftIO $ putStrLn "Leaving SINH..."
  liftIO $ exitSuccess

-- :help command
help :: a -> Repl ()
help _ = do
  liftIO $ putStrLn "SINH language has following utility commands:"
  liftIO $ putStrLn "\t1. :load <filename> \t To load a program file"
  liftIO $ putStrLn "\t   :l <filename>"
  liftIO $ putStrLn "\t2. :quit \t\t To quit the shell command"
  liftIO $ putStrLn "\t   :q"
  liftIO $ putStrLn "\t3. :help \t\t Get full list of utility functions"
  liftIO $ putStrLn "\t   :h"


-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [(":load"  , fileCompleter), (":l"  , fileCompleter)]

-- Default tab completer
comp :: Monad m => WordCompleter m
comp n = do
  let cmds = [":load", ":quit", ":help"]
  return $ filter (isPrefixOf n) cmds


options :: [(String, [String] -> Repl ())]
options = [
            ("load", load), ("l", load), 
            ("quit", quit), ("q", quit),
            ("help", help), ("h", help)]


-- Entry point

completer :: CompleterStyle IO
completer = Prefix (wordCompleter comp) defaultMatcher

banner :: Repl String
banner = pure $ blue ++ "sinh>> " ++ colorReset

ini :: Repl ()
ini = do
  liftIO $ putStrLn $ "Welcome to SINH! Curent version: "  ++ version
  liftIO $ putStrLn $ "Type :help to get full list of utility functions"

shell :: Repl a -> IO ()
shell pre =  evalRepl banner exec options (Just ':') completer pre

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> shell (ini)
    [fname] -> load2 fname
    _ -> do
      putStrLn $ yellow ++ "Invalid arguments! Proper use:" ++ colorReset
      putStrLn "\t\t<stack build> sinh <filename>"



  -- src <- readFile . head $ args
  -- --print src
  -- let ast = parseExpr src
  -- -- print ast
  -- if checkProgram ast
  --   then print . execute $ ast
  --   else error "You have type error in your program"
