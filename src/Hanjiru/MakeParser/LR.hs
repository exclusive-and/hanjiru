module Hanjiru.MakeParser.LR where

import Data.Foldable (foldrM)
import Data.Hanjiru.Fix
import Data.Hanjiru.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Hanjiru.MakeParser.Lookahead
import Hanjiru.MakeParser.Rules

-- | An LR(1) parse state.
--
-- 
newtype LR a = LR (Map (Item a) (Set a))
    deriving (Eq, Ord, Semigroup)

closure :: Ord a => LR a -> LR a
closure = saturate go
    where
        go lr@(LR items) = foldrM (uncurry expandItemInClosure) lr (Map.toList items)

-- | Expand a non-terminal item, and add its expansion to the closure.
expandItemInClosure :: Ord a => Item a -> Set a -> LR a -> Changes (LR a)
expandItemInClosure item la lr = nonterm (pure lr) go item
    where
        go tok alts beta =
            let
                la' = list la (\a _ -> first a) beta
                inner alt@(Alt gamma) = extend (toItem tok alt gamma) la'
            in
                foldrM inner lr alts

-- | Extend an LR state with an item and its lookahead.
--
-- Only results in a change if:
--
--    * The item was not already in the LR state, or
--
--    * The item was in the LR state, but its lookahead set is different.
extend :: Ord a => Item a -> Set a -> LR a -> Changes (LR a)
extend item la (LR items) = do
    let (maybeOld, xs') = Map.insertLookupWithKey (\_ -> Set.union) item la items
    case maybeOld of
        Nothing -> changed (LR xs')
        Just old
            | Set.isSubsetOf la old -> pure (LR xs')
            | otherwise             -> changed (LR xs')

gotos :: Ord a => LR a -> Map (View a) (LR a)
gotos (LR items) = closure <$> Map.fromListWith (<>) items'
    where
        items' =
            [ (x, LR (Map.singleton (Item tok alt xs) la))
            | (Item tok alt (x:xs), la) <- Map.toList items
            ]