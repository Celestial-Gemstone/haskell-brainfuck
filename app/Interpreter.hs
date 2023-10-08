module Interpreter (Brainfuck, BrainfuckInstruction(..), BrainfuckStatement(..), interpret) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.State (StateT (..), get, gets, modify)

import Data.Char (chr, ord)

import Tape (BrainfuckTape, apply, curr, next, prev, set)


data BrainfuckInstruction
  = Increment | Decrement
  | PointerRight | PointerLeft
  | Print | Read
  deriving(Show, Eq)

data BrainfuckStatement
  = Instruction BrainfuckInstruction
  | Loop Brainfuck
  deriving(Show)

type Brainfuck = [BrainfuckStatement]

interpret :: Brainfuck -> StateT BrainfuckTape IO ()
interpret = foldr ((>>) . process_statement) (pure ())
  where process_statement (Instruction instr) = handle instr
        process_statement (Loop bf)           = loop bf

loop :: Brainfuck -> StateT BrainfuckTape IO ()
loop bf = do tape <- get
             unless (curr tape == 0) $ do
                 interpret bf
                 loop bf

handle :: BrainfuckInstruction -> StateT BrainfuckTape IO ()
handle Print = gets (putChar . chr . curr) >>= lift
handle Read = modifyM (\t -> set t . ord <$> getChar)
handle instr = modify $ case instr of
  Increment    -> apply succ
  Decrement    -> apply pred
  PointerRight -> next
  PointerLeft  -> prev

-- this should be changed once the version of base we are
-- using has this function
modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = StateT $ \ s -> do
    s' <- f s
    return ((), s')
