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
