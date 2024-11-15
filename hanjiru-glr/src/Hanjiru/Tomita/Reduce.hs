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
reduce :: ParseStack a -> Reduction -> [(Reduced a, ParseStack a)]
reduce stack (MkReduction tok numConsumed) =
    consumeN numConsumed stack $
        \state parses -> Reduced state tok (Production tok numConsumed parses)

data Reduced a = Reduced Int Token (Parse a)

pack :: forall a. Eq a => (Reduced a, ParseStack a) -> [ParseStack a] -> [ParseStack a]
pack (Reduced state _ parse, stack) = go []
    where
    go xs [] = insert (push 0 state parse stack) (reverse xs)
    go xs (candidate:stacks) =
        case tryPack candidate of
            Just packed -> reverse xs ++ (packed:stacks)
            Nothing     -> go (candidate:xs) stacks
    
    tryPack (ParseHead uniq state' parse' stack')
        | state == state'
        , stack == stack' =
            Just (push uniq state (ambiguity parse parse') stack)
    tryPack _ = Nothing