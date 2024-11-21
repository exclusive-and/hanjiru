module Hanjiru.LR where

import Control.Arrow (first)
import Control.Monad.State
import Data.List (partition)

{-
lrMain :: Grammar token -> LR token
lrMain = 

data LR token = 
-}
data Grammar token = Grammar [Production token]

data LRState token = LRState [Item token]

-- | 
closure :: forall token. Eq token => Grammar token -> [Item token] -> [Item token]
closure (Grammar productions) items =
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

data Production token = Production token [token]

data Item token = Item token [token] [token]
    deriving (Eq, Show)

mkItem :: Production token -> Item token
mkItem (Production lhs rhs) = Item lhs [] rhs