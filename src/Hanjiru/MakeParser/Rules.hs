module Hanjiru.MakeParser.Rules where

import Data.Hanjiru.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Semigroup
import Data.Set (Set)
import Data.Set qualified as Set

-- | A grammatical production rule from the BNF.

data Rule a = Rule a (NonEmpty a)
    deriving (Eq, Ord, Show)

-- | The grammar of a language.

data Grammar a =
        Grammar (Map a (NonEmpty (Rule a))) -- Map of rules
                (Rule a)                    -- Goal rule
    deriving Show

-- | The lazy expansion of a token. Likely infinite, so make extra sure that code
--   dealing with these terminates!

data View a = Term a | NonTerm a (NonEmpty (Alt a))

-- | Two Views are equal only if they view the exact same token.

instance Eq a => Eq (View a) where
    Term    tok0    ==  Term    tok1    = tok0 == tok1
    NonTerm tok0 _  ==  NonTerm tok1 _  = tok0 == tok1
    _               ==  _               = False

-- | Views of terminal symbols are /always/ less than those of non-terminals.
--   Otherwise, we follow the same order as the viewed tokens.

instance Ord a => Ord (View a) where
    compare x y =
        case (x, y) of
            (Term    _      , NonTerm _    _) -> LT
            (NonTerm _    _ , Term    _     ) -> GT
            (Term    tok0   , Term    tok1  ) -> compare tok0 tok1
            (NonTerm tok0 _ , NonTerm tok1 _) -> compare tok0 tok1

instance Show a => Show (View a) where
    show (Term    tok   ) = show tok
    show (NonTerm tok _ ) = show tok

-- | The token expansion of the RHS of a single rule. Represents one potential parse
--   of a non-terminal symbol.

newtype Alt a = Alt (NonEmpty (View a))
    deriving (Eq, Ord, Show)

-- | Expand a token into its 'View'. The expansion is transitive: if a token expands into
--   more tokens, then those tokens are also expanded.

view :: Ord a => Map a (NonEmpty (Rule a)) -> a -> View a
view grammar = goView where
    goView tok =
        case Map.lookup tok grammar of
            Nothing     -> Term tok
            Just rules  -> NonTerm tok (goAlt <$> rules)
    
    goAlt (Rule _ rhs) = Alt (goView <$> rhs)

viewAll :: Ord a => Map a (NonEmpty (Rule a)) -> Map a (View a)
viewAll grammar = Map.mapWithKey (const . view grammar) grammar

symbol :: View a -> a
symbol (Term tok)       = tok
symbol (NonTerm tok _)  = tok

-- | The incomplete parse of a single grammar rule.
--
-- Includes the expanded form of the rule being parsed, along with a list of the tokens
-- still needed to complete the parse.

data Item a = Item a (Alt a) [View a]
    deriving (Eq, Ord)

instance Show a => Show (Item a) where
    show (Item tok _ xs) = "<" ++ show tok ++ " => " ++ show xs ++ ">"

toItem :: a -> Alt a -> NonEmpty (View a) -> Item a
toItem tok alt xs = Item tok alt (NE.toList xs)

toRule :: Item a -> Rule a
toRule (Item tok (Alt toks) _) = Rule tok (go <$> toks)
    where
        go (Term tok)       = tok
        go (NonTerm tok _)  = tok

uncons :: Item a -> Maybe (View a, Item a)
uncons (Item tok alt xs) =
    case xs of
        []      -> Nothing
        x : xs' -> Just (x, Item tok alt xs')

-- | Apply a function to non-terminal symbols at the head of an item.

nonterm :: b -> (a -> NonEmpty (Alt a) -> [View a] -> b) -> Item a -> b
nonterm b f (Item _ _ xs) = list b go xs
    where
        go (Term _)             = const b
        go (NonTerm tok alts)   = f tok alts

-- | Expand the next expected symbol at the front of an item into new items.

expandFront :: Item a -> [Item a]
expandFront = nonterm [] (\tok alts _ -> map (go tok) $ NE.toList alts)
    where
        go tok alt@(Alt xs) = toItem tok alt xs