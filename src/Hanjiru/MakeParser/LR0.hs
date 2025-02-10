module Hanjiru.MakeParser.LR0 where

import Control.Arrow
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Foldable (foldrM)
import Data.Hanjiru.MapGraph
import Data.Hanjiru.Strategies
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Hanjiru.MakeParser.Rules
import Hanjiru.Tables

-- | An LR(0) parse state.

newtype LR a = LR (Set (Item a))
    deriving (Eq, Ord, Semigroup)

-- | Compute the closure of an LR(0) parse state via breadth-first search.

closure :: Ord a => LR a -> LR a
closure (LR lr) = LR (bfsM go worklist0 `execState` lr)
    where
        go item = do
            items <- lift get
            if item `Set.member` items
                then pure ()
                else do
                    lift $ put $ Set.insert item items
                    tell $ expandFront item

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

-- | Construct LR(0) action and goto tables from a grammar.

makeLr0 :: (Ord a, Show a) => Grammar a -> (ActionTable a, GotoTable a)
makeLr0 (Grammar grammar goal) =
    let
        MapGraph edges nodes = unfold (Map.toList . successors) $ startLr0 grammar goal
        reductions = Map.map collectReductions nodes
        (shifts, gotos) = collectShiftsAndGotos edges
    in
        (actionTable goal (Map.map (map toReduction) reductions) shifts, gotoTable gotos)

-- | Compute the initial LR(0) state to explore from.

startLr0 :: Ord a => Map a (NonEmpty (Rule a)) -> Rule a -> LR a
startLr0 grammar (Rule tok xs) =
    let
        xs' = view grammar <$> xs
    in
        closure $ LR $ Set.singleton $ toItem tok (Alt xs') xs'

collectReductions :: LR a -> [Rule a]
collectReductions (LR items) =
    [ toRule item | item@(Item _ _ []) <- Set.toList items ]

collectShiftsAndGotos :: Ord a => Map (Int, View a) Int -> (GotoMap a, GotoMap a)
collectShiftsAndGotos = Map.foldrWithKey go (mempty, mempty)
    where
        go (v, e) w (shifts, gotos) =
            case e of
                NonTerm tok _   -> (shifts, Map.insert (v, tok) w gotos)
                Term tok        -> (Map.insert (v, tok) w shifts, gotos)