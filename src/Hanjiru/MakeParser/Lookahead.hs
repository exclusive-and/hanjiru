module Hanjiru.MakeParser.Lookahead where

import Control.Monad.Trans.State
import Data.Hanjiru.Fix
import Data.Hanjiru.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Semigroup
import Data.Set (Set)
import Data.Set qualified as Set
import Hanjiru.MakeParser.Rules

newtype LA a = LA (Set (Maybe a))
    deriving (Eq, Ord, Semigroup)

instance Show a => Show (LA a) where
    show (LA xs) = show (catMaybes $ Set.toList xs)

empty :: LA a
empty = LA Set.empty

nothing :: LA a
nothing = LA (Set.singleton Nothing)

singleton :: a -> LA a
singleton = LA . Set.singleton . Just

plus :: Ord a => LA a -> LA a -> LA a
plus (LA x) (LA y)
    | Nothing `Set.member` x = LA (x <> y)
    | otherwise              = LA x

-- | Calculate a set of prefix terminals for each non-terminal in the language.
firstSets :: Ord a => Map a (NonEmpty (Rule a)) -> Map a (LA a)
firstSets rules =
    Map.map fst
        $ saturate (\xs -> traverse (go xs) xs)
        $ Map.map (empty,) rules
    where
        go xs x@(old, alts)
            | new == old = pure x
            | otherwise  = changed (new, alts)
            where
                new = foldr (<>) empty (goAlt xs <$> alts)

        goAlt xs (Rule _ (x :| _)) =
            case Map.lookup x xs of
                Just (s, _) -> s
                Nothing     -> singleton x

first :: Ord a => Map a (LA a) -> LA a -> [View a] -> LA a
first lookaheads la = list la (\x _ -> go $ symbol x)
    where
        go tok =
            case Map.lookup tok lookaheads of
                Just la' -> la'
                Nothing  -> singleton tok