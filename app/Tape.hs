module Tape (BrainfuckTape, next, prev, curr, apply, zeroes, set) where

data Tape a = Tape [a] a [a]
type BrainfuckTape = Tape Int

zeroes :: Num a => Tape a
zeroes = Tape (repeat 0) 0 (repeat 0)

curr :: Tape a -> a
curr (Tape _ n _) = n

next :: Tape a -> Tape a
next (Tape ls n (r:rs)) = Tape (n:ls) r rs
next t                  = t

prev :: Tape a -> Tape a
prev (Tape (l:ls) n rs) = Tape ls l (n:rs)
prev t                  = t

apply :: (a -> a) -> Tape a -> Tape a
apply f (Tape ls n rs) = Tape ls (f n) rs

set :: Tape a -> a -> Tape a
set (Tape ls _ rs) x = Tape ls x rs
