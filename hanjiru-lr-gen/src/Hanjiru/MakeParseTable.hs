module Hanjiru.MakeParseTable where

import Control.Monad.Trans.Writer
import Data.Foldable (foldrM)
import Data.Hanjiru.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Semigroup
import Data.Set (Set)
import Data.Set qualified as Set

data Rule a = Rule a (NonEmpty a)
    deriving (Eq, Ord, Show)

-- | The lazy expansion of a token. Likely infinite, so make extra sure that code
--   dealing with these terminates!
data View a = Term a | NonTerm a (NonEmpty (Alt a))

instance Eq a => Eq (View a) where
    Term    tok0    ==  Term    tok1    = tok0 == tok1
    NonTerm tok0 _  ==  NonTerm tok1 _  = tok0 == tok1
    _               ==  _               = False

instance Ord a => Ord (View a) where
    compare x y =
        case (x, y) of
            (Term    _      , NonTerm _    _) -> LT
            (NonTerm _    _ , Term    _     ) -> GT
            (Term    tok0   , Term    tok1  ) -> compare tok0 tok1
            (NonTerm tok0 _ , NonTerm tok1 _) -> compare tok0 tok1

data Alt a = Alt (NonEmpty (View a))
    deriving (Eq, Ord)

-- | Expand a token into its 'View'. The expansion is transitive: if a token expands into
--   more tokens, then those tokens are also expanded.
view :: Ord a => Map a (NonEmpty (Rule a)) -> a -> View a
view grammar = goView where
    goView tok =
        case Map.lookup tok grammar of
            Nothing     -> Term tok
            Just rules  -> NonTerm tok (goAlt <$> rules)
    
    goAlt (Rule _ rhs) = Alt (goView <$> rhs)

-- | Drill down into a symbol to get the set of all terminals that can prefix it.
first :: Ord a => View a -> Set a
first (Term tok)       = Set.singleton tok
first (NonTerm _ alts) =
    sconcat $ (\(Alt rhs) -> first (NE.head rhs)) <$> alts

-- | 
data Item a = Item a (Alt a) [View a]
    deriving (Eq, Ord)

uncons :: Item a -> Maybe (View a, Item a)
uncons (Item tok alt xs) =
    case xs of
        []      -> Nothing
        x : xs' -> Just (x, Item tok alt xs')

toItem :: a -> Alt a -> NonEmpty (View a) -> Item a
toItem tok alt xs = Item tok alt (NE.toList xs)

toRule :: Item a -> Rule a
toRule (Item tok (Alt toks) _) = Rule tok (go <$> toks)
    where
        go (Term tok)       = tok
        go (NonTerm tok _)  = tok

newtype LR a = LR (Map (Item a) (Set a))
    deriving (Eq, Ord, Semigroup)

type Change = Writer Any

changed :: a -> Change a
changed x = tell (Any True) >> pure x

saturate :: (a -> Change a) -> a -> a
saturate f = loop where
    loop x =
        case runWriter (f x) of
            (y, Any True) -> loop y
            (y, _       ) -> y

closure :: Ord a => LR a -> LR a
closure = saturate closure1

-- | 
closure1 :: Ord a => LR a -> Change (LR a)
closure1 (LR items) = LR <$> foldrM go items items'
    where
        items' =
            [ (toItem tok alt gamma, la')
            | (Item _ _ (NonTerm tok alts:beta), la) <- Map.toList items
            , alt@(Alt gamma)                        <- NE.toList alts
            , let la' = list la (\a _ -> first a) beta
            ]
        
        go (item, la) xs = do
            let (maybeOld, xs') = Map.insertLookupWithKey (\_ -> Set.union) item la xs
            case maybeOld of
                Nothing -> changed xs'
                Just old
                    | Set.isSubsetOf la old -> pure xs'
                    | otherwise             -> changed xs'

gotos :: Ord a => LR a -> Map (View a) (LR a)
gotos (LR items) = closure <$> Map.fromListWith (<>) items'
    where
        items' =
            [ (x, LR (Map.singleton (Item tok alt xs) la))
            | (Item tok alt (x:xs), la) <- Map.toList items
            ]

instance Show a => Show (View a) where
    show (Term    tok   ) = show tok
    show (NonTerm tok _ ) = show tok

deriving instance Show a => Show (Alt a)

instance Show a => Show (Item a) where
    show (Item tok _ xs) = "[" ++ show tok ++ " => " ++ show xs ++ "]"