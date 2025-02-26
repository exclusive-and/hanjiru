module Hanjiru.Tomita.Stack where

import Hanjiru.Tomita.Parse

data ParseStack token a = ParseStack Int Int [ParseTail token a]
    deriving (Eq, Show)

instance (Eq token, Eq a) => Ord (ParseStack token a) where
    compare stack0 stack1 = compare (height stack1) (height stack0)

pattern ParseHead :: Int -> Int -> Parse token a -> ParseStack token a -> ParseStack token a
pattern ParseHead uniq state parse stack =
    ParseStack uniq state [ParseTail parse stack]

data ParseTail token a = ParseTail (Parse token a) (ParseStack token a)
    deriving (Eq, Ord, Show)

peek :: ParseStack token a -> [Parse token a]
peek (ParseStack _ _ tails) = map (\(ParseTail parse _) -> parse) tails

top :: ParseStack token a -> Int
top (ParseStack _ state _) = state

height :: ParseStack token a -> Int
height (ParseStack h _ _) = h

push :: Int -> Int -> Parse token a -> ParseStack token a -> ParseStack token a
push _ state parse stack = ParseHead (height stack + 1) state parse stack

consumeN ::
       Int
    -> ParseStack token a
    -> (Int -> [Parse token a] -> b)
    -> [(b, ParseStack token a)]
consumeN n0 stack0 f = go n0 [] stack0
    where
        go 0 xs stack = [(f (top stack) xs, stack)]
        go n xs (ParseStack _ _ tails) =
            concatMap (\(ParseTail parse stack) -> go (n - 1) (parse:xs) stack) tails