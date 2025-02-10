module Hanjiru.Tables where

import Data.Map (Map)
import Data.Map qualified as Map
import Hanjiru.MakeParser.Rules

data Action token =
      Accept
    | Error
    | Reduce [Reduction token]
    | Shift  [Reduction token] Int

-- | A reduction pops some tokens from the parse stack, and combines them into a new token.

data Reduction token =
        MkReduction token   -- Which token do I reduce to?
                    Int     -- How many tokens do I pop from the stack?
{-
                    Int     -- Where can I find my code?
-}
    deriving (Eq, Ord)

toReduction :: Rule a -> Reduction a
toReduction (Rule tok xs) = MkReduction tok (length xs)

type ActionTable token    = Int -> token -> Action token
type GotoTable   token    = Int -> token -> Int

type ActionMap a = Map Int [Action a]

type GotoMap a = Map (Int, a) Int

actionTable :: Ord a => Rule a -> Map Int [Reduction a] -> Map (Int, a) Int -> ActionTable a
actionTable (Rule goal _) reductionMap shiftMap state0 tok =
    case (shift, reductions) of
        (Nothing    , Nothing) -> Error
        (Nothing    , Just rs)  | [MkReduction tok _] <- rs
                                , tok == goal  -> Accept
                                | otherwise    -> Reduce rs
        (Just state1, Nothing) -> Shift [] state1
        (Just state1, Just rs) -> Shift rs state1
    where
        reductions  = Map.lookup state0 reductionMap
        shift       = Map.lookup (state0, tok) shiftMap

gotoTable :: (Ord a, Show a) => Map (Int, a) Int -> GotoTable a
gotoTable gotoMap state0 tok = Map.findWithDefault state0 (state0, tok) gotoMap