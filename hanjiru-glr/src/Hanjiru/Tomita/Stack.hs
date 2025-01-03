module Hanjiru.Tomita.Stack where

import Hanjiru.Tomita.Parse

data ParseStack a = ParseStack Int Int [ParseTail a]
    deriving Show

deriving instance Eq a => Eq (ParseStack a)

instance Eq a => Ord (ParseStack a) where
    compare stack0 stack1 = compare (height stack1) (height stack0)

pattern ParseHead :: Int -> Int -> Parse a -> ParseStack a -> ParseStack a
pattern ParseHead uniq state parse stack =
    ParseStack uniq state [ParseTail parse stack]

data ParseTail a = ParseTail (Parse a) (ParseStack a)
    deriving Show

deriving instance Eq a => Eq (ParseTail a)

peek :: ParseStack a -> [Parse a]
peek (ParseStack _ _ tails) =
    map (\(ParseTail parse _) -> parse) tails

top :: ParseStack a -> Int
top (ParseStack _ state _) = state

height :: ParseStack a -> Int
height (ParseStack h _ _) = h

push :: Int -> Int -> Parse a -> ParseStack a -> ParseStack a
push _ state parse stack =
    ParseHead (height stack + 1) state parse stack

consumeN :: Int -> ParseStack a -> (Int -> [Parse a] -> b) -> [(b, ParseStack a)]
consumeN n0 stack0 f = go n0 [] stack0
    where
    go 0 xs stack = [(f (top stack) xs, stack)]
    go n xs (ParseStack _ _ tails) =
        concatMap (\(ParseTail parse stack) -> go (n - 1) (parse:xs) stack) tails