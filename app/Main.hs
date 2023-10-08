module Main (main) where

import Control.Monad.Trans.State (execStateT)
import Data.Maybe (mapMaybe)
import Interpreter
    ( Brainfuck
    , BrainfuckInstruction (..)
    , BrainfuckStatement (..)
    , interpret
    )
import Tape (zeroes)

-- Parser
data BrainfuckCommand
  = LoopStart | LoopEnd
  | Command BrainfuckInstruction
  deriving (Show, Eq)

data ParseError = UnclosedLoop | UnexpectedlyClosedLoop
  deriving Show

lexBrainfuck :: String -> [BrainfuckCommand]
lexBrainfuck = mapMaybe (`lookup` commands)

commands :: [(Char, BrainfuckCommand)]
commands =
  [ ('+', Command Increment)
  , ('-', Command Decrement)
  , ('>', Command PointerRight)
  , ('<', Command PointerLeft)
  , ('.', Command Print)
  , (',', Command Read)
  , ('[', LoopStart)
  , (']', LoopEnd) ]

parseBrainfuck :: [BrainfuckCommand] -> Either ParseError Brainfuck
parseBrainfuck [] = Right []
parseBrainfuck (command : rest) =
  case command of
    LoopEnd -> Left UnexpectedlyClosedLoop
    LoopStart -> let (result, r) = parseLoop rest in
                    result >>= (\bf -> prepend (Loop bf) (parseBrainfuck r))
    Command instr -> prepend (Instruction instr) (parseBrainfuck rest)


parseLoop :: [BrainfuckCommand] -> (Either ParseError Brainfuck, [BrainfuckCommand])
parseLoop [] = (Left UnclosedLoop, [])
parseLoop (command : rest) = case command of
    LoopEnd       -> (Right [], rest)
    LoopStart     -> parseInnerLoop rest
    Command instr -> mapFirst (prepend (Instruction instr)) (parseLoop rest)

parseInnerLoop :: [BrainfuckCommand] -> (Either ParseError Brainfuck, [BrainfuckCommand])
parseInnerLoop rest = let (result, r) = parseLoop rest in
    case result of
        Right bf -> mapFirst (prepend (Loop bf)) (parseLoop r)
        Left x   -> (Left x, r)


prepend :: BrainfuckStatement -> Either ParseError Brainfuck -> Either ParseError Brainfuck
prepend st bf = (st :) <$> bf

mapFirst :: (a -> a') -> (a, b) -> (a', b)
mapFirst f (x, y) = (f x, y)

run :: String -> IO ()
run str = case parseBrainfuck (lexBrainfuck str) of
  Right bf -> do
    _ <- execStateT (interpret bf) zeroes
    putStrLn ""
  Left err -> print err

main :: IO ()
main = run "++++++++[>+>++++<<-]>++>>+<[-[>>+<<-]+>>]>+[-<<<[->[+[-]+>++>>>-<<]<[<]>>++++++[<<+++++>>-]+<<++.[-]<<]>.>+[>>]>+]"
-- main = run "++++[>++++<-]>."
