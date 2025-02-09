module Hanjiru.Tomita.Reduce where

import Hanjiru.Tables
import Hanjiru.Tomita.Parse
import Hanjiru.Tomita.Stack

import Data.List (insert)

-- | 
reduce :: ParseStack token a -> Reduction token -> [(Reduced token a, ParseStack token a)]
reduce stack (MkReduction tok numConsumed) =
    consumeN numConsumed stack $
        \state parses -> Reduced state tok (Production tok numConsumed parses)

data Reduced token a = Reduced Int token (Parse token a)

pack :: forall a token.
       (Ord token, Eq a)
    => (Reduced token a, ParseStack token a)
    -> [ParseStack token a]
    -> [ParseStack token a]
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