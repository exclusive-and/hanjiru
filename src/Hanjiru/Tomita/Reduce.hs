module Hanjiru.Tomita.Reduce where

import Data.List (insert)
import Hanjiru.Tables
import Hanjiru.Tomita.Parse
import Hanjiru.Tomita.Stack

-- | 
reduce :: ParseStack token a -> Reduction token -> [(Reduced token a, ParseStack token a)]
reduce stack (MkReduction tok numConsumed) =
    consumeN numConsumed stack $
        \state parses -> Reduced state tok (Production tok numConsumed parses)

data Reduced token a = Reduced Int token (Parse token a)
