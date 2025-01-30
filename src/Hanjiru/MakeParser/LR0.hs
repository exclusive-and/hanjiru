module Hanjiru.MakeParser.LR0 where

import Control.Arrow
import Control.Monad.Trans.State
import Data.Foldable (foldrM)
import Data.Hanjiru.MapGraph
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
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

data Action a   = Shift  a Int
                | Reduce (Rule a)

type ActionMap a = Map Int [Action a]

type GotoMap a = Map (Int, a) Int

makeLr0 :: Ord a => Map a (NonEmpty (Rule a)) -> Rule a -> (ActionMap a, GotoMap a)
makeLr0 grammar goal =
    let
        MapGraph edges nodes = unfold (Map.toList . successors) $ startLr0 grammar goal
    in
        first (Map.unionWith (<>) (Map.map reductions nodes)) $ collectShiftsAndGotos edges

startLr0 :: Ord a => Map a (NonEmpty (Rule a)) -> Rule a -> LR a
startLr0 grammar (Rule tok xs) =
    let
        xs' = view grammar <$> xs
    in
        closure $ LR $ Set.singleton $ toItem tok (Alt xs') xs'

reductions :: LR a -> [Action a]
reductions (LR items) = [ Reduce (toRule item) | item@(Item _ _ []) <- Set.toList items ]

collectShiftsAndGotos :: Ord a => Map (Int, View a) Int -> (ActionMap a, GotoMap a)
collectShiftsAndGotos = Map.foldrWithKey go (mempty, mempty)
    where
        go (v, e) w (shifts, gotos) =
            case e of
                NonTerm tok _   -> (shifts, Map.insert (v, tok) w gotos)
                Term tok        -> (Map.insertWith (<>) v [Shift tok w] shifts, gotos)