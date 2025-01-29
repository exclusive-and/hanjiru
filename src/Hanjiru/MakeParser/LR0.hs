module Hanjiru.MakeParser.LR0 where

import Data.Hanjiru.Strategies
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Hanjiru.MakeParser.Rules

-- | An LR(0) parse state.

newtype LR a = LR (Set (Item a))
    deriving (Eq, Ord, Semigroup)

-- | Compute the closure of an LR(0) parse state via breadth-first search.

closure :: Ord a => LR a -> LR a
closure (LR lr) = LR (go lr worklist0)
    where
        go acc [] = acc
        go acc (item:worklist) =
            if item `Set.member` acc
                then go acc worklist
                else go (Set.insert item acc) (expandFront item ++ worklist)

        worklist0 = concatMap expandFront $ Set.toList lr

-- | Compute a map of LR(0) states to enter, depending on the next token that gets parsed.

successors :: Ord a => LR a -> Map (View a) (LR a)
successors (LR items) =
    closure <$> Map.fromListWith (<>) items'
    where
        items' =
            [ (x, LR (Set.singleton $ Item tok alt xs))
            | Item tok alt (x:xs) <- Set.toList items
            ]