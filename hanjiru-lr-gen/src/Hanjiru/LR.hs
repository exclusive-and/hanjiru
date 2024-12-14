module Hanjiru.LR where

import Control.Arrow (first)
import Control.Monad.State
import Data.List (partition)

{-
lrMain :: Grammar token -> LR token
lrMain = 

data LR token = 
-}


data Item token = Item  token   -- ^ what this item looks for
                        (Rule token)
                                -- ^ rule to match against
                        token   -- ^ lookahead word
    deriving (Eq, Show)

data Rule token = Rule  [token] -- ^ tokens seen before *
                        [token] -- ^ tokens expected after *

toItem :: Production token -> token -> Item token
toItem (Production lhs rhs) lookahead = Item lhs (Rule [] rhs) lookahead

withPrefixes :: ProdMap token -> Production token -> (token -> r) -> [r]
withPrefixes pm (Production lhs rhs) f = go rhs
    where
    go []      = []
    go (b : _) =
        case Map.lookup pm b of
            Nothing     -> f b
            Just prods  -> concatMap (\p -> withPrefixes pm p f) prods

data Grammar token = Grammar [Production token]

data Production token = Production token [token]

data LRState token = LRState [Item token]

type ProdMap token = Map token [Production token]

search :: ProdMap token -> [token] -> [Production token]
search _ [] = []
search prods (b : _) = Map.lookup prods b

expandItem :: ProdMap a -> Item a -> [Item a]
expandItem pm (Item _ rule lookahead) = expandRule pm rule

expandRule :: ProdMap a -> Rule a -> [Item a]
expandRule pm (Rule _ rhs) = go rhs
    where
    go []    = []
    go (b:_) =
        case Map.lookup pm b of
            Nothing -> 

-- | 
closure :: Eq token => ProdMap token -> [Item token] -> [Item token]
closure pm items =
        evalState (go items) productions
    where
    go :: [Item token] -> State [Production token] [Item token]
    go = fmap concat . traverse closeOver

    closeOver :: Item token -> State [Production token] [Item token]
    closeOver item =
        case item of
            Item _ _ []    -> pure [item]
            Item _ _ (x:_) -> compatible x >>= fmap (item:) . go

    compatible :: token -> State [Production token] [Item token]
    compatible tok = state $
        first (map mkItem) . partition (\(Production lhs _) -> tok == lhs)

goto :: [Item token] -> token -> [Item token]
goto 


mkItem :: Production token -> Item token
mkItem (Production lhs rhs) = Item lhs [] rhs