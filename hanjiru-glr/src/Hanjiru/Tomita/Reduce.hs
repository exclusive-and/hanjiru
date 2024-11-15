module Hanjiru.Tomita.Reduce where

import Hanjiru.Token
import Hanjiru.Tomita.Parse
import Hanjiru.Tomita.Stack

import Data.List (insert)

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

pack :: forall a. Eq a => Int -> Parse a -> ParseStack a -> [ParseStack a] -> [ParseStack a]
pack state parse stack = go id
    where
    go :: ([ParseStack a] -> [ParseStack a]) -> [ParseStack a] -> [ParseStack a]
    go f [] = insert (push 0 state parse stack) (f [])
    go f (candidate:stacks) =
        case tryPack candidate of
            Just packed -> insert packed (f stacks)
            Nothing     -> go ((candidate:) . f) stacks
    
    tryPack (ParseHead uniq state' parse' stack')
        | state == state'
        , stack == stack' =
            Just (push uniq state (ambiguity parse parse') stack)
    tryPack _ = Nothing