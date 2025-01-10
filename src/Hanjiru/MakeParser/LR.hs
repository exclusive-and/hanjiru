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
newtype LR a = LR (Map (Item a) (LA a))
    deriving (Eq, Ord, Semigroup)

closure :: Ord a => Map a (LA a) -> LR a -> LR a
closure lookaheads = saturate go
    where
        go lr@(LR items) =
            foldrM
                (uncurry $ expandItemInClosure lookaheads)
                lr
                (Map.toList items)

-- | Expand a non-terminal item, and add its expansion to the closure.
expandItemInClosure :: Ord a => Map a (LA a) -> Item a -> LA a -> LR a -> Changes (LR a)
expandItemInClosure lookaheads item la lr = nonterm (pure lr) go item
    where
        go tok alts beta =
            let
                la' = first lookaheads la beta
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
extend :: Ord a => Item a -> LA a -> LR a -> Changes (LR a)
extend item (LA la) (LR items) = do
    let (maybeOld, xs') = Map.insertLookupWithKey (\_ -> (<>)) item (LA la) items
    case maybeOld of
        Nothing -> changed (LR xs')
        Just (LA old)
            | Set.isSubsetOf la old -> pure (LR xs')
            | otherwise             -> changed (LR xs')

gotos :: Ord a => Map a (LA a) -> LR a -> Map (View a) (LR a)
gotos lookaheads (LR items) = closure lookaheads <$> Map.fromListWith (<>) items'
    where
        items' =
            [ (x, LR (Map.singleton (Item tok alt xs) la))
            | (Item tok alt (x:xs), la) <- Map.toList items
            ]