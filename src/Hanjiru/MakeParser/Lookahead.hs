module Hanjiru.MakeParser.Lookahead where

import Control.Monad.Trans.State
import Data.Hanjiru.Fix
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Semigroup
import Data.Set (Set)
import Data.Set qualified as Set
import Hanjiru.MakeParser.Rules

-- | Calculate a set of prefix terminals for each non-terminal in the language.
firstSets :: Ord a => Map a (NonEmpty (Rule a)) -> Map a (Set a)
firstSets rules =
    Map.map fst
        $ saturate (\xs -> traverse (go xs) xs)
        $ Map.map (Set.empty,) rules
    where
        go xs x@(old, alts)
            | new == old = pure x
            | otherwise  = changed (new, alts)
            where
                new = foldr (<>) Set.empty (goAlt xs <$> alts)

        goAlt xs (Rule _ (x :| _)) =
            case Map.lookup x xs of
                Just (s, _) -> s
                Nothing     -> Set.singleton x

-- | Drill down into a symbol to get the set of all terminals that can prefix it.
first :: Ord a => View a -> Set a
first x = evalState (go x) Set.empty
    where
        go (Term tok) = do
            visited <- get
            put (Set.insert tok visited)
            pure (Set.singleton tok)
        go (NonTerm tok alts) = do
            visited <- get
            if Set.member tok visited
                then pure Set.empty
                else do
                    put (Set.insert tok visited)
                    sconcat <$> traverse (\(Alt rhs) -> go (NE.head rhs)) alts
