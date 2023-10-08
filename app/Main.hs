module Main (main) where

import Control.Monad.Trans.State (execStateT)
import Tape (zeroes)
import Interpreter (interpret)
import Parser (parse)
import System.Exit (die, exitSuccess)
import System.Environment (getArgs)


run :: String -> IO ()
run str = case parse str of
  Right bf -> do
    _ <- execStateT (interpret bf) zeroes
    putStrLn ""
    exitSuccess
  Left err -> die (show err)

main :: IO ()
main = do args <- getArgs
          handle args

handle :: [String] -> IO ()
handle ("file" : rest) = case rest of
  [file] -> readFile file >>= run
  _      -> die "file takes one argument"
handle ("string" : rest) = case rest of
  [bf] -> run bf
  _    -> die "string takes one argument"
handle _ = die "Expected either one of 'file <file>' and 'string <brainfuck code>'"

