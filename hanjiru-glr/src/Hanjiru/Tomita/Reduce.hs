module Hanjiru.Tomita.Reduce where

import Hanjiru.Token
import Hanjiru.Tomita.Parse
import Hanjiru.Tomita.Stack

import Data.List (insert, sortOn)
import Debug.Trace

-- | A reduction pops some tokens from the parse stack, and combines them into a new token.
data Reduction =
        MkReduction Token   -- Which token do I reduce to?
                    Int     -- How many tokens do I pop from the stack?
{-
                    Int     -- Where can I find my code?
-}

-- | 
reduce :: Reduction -> ParseStack a -> [(Parse a, ParseStack a)]
reduce (MkReduction tok numConsumed) stack =
    consumeN numConsumed stack (Production tok numConsumed)

pack :: Eq a => Int -> Parse a -> ParseStack a -> [ParseStack a] -> [ParseStack a]
pack state0 parse0 stack0 = go []
    where
    go xs [] = insert (ParseHead (height stack0 + 1) state0 parse0 stack0) (reverse xs)
    go xs (stack1:stacks) =
        case stack1 of
            ParseHead uniq state1 parse1 stack2
                | state0 == state1
                , stack0 == stack2 ->
                    insert (ParseHead uniq state0 (ambiguity parse0 parse1) stack0) (reverse xs ++ stacks)
            _ -> go (stack1:xs) stacks