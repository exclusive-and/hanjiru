module Hanjiru.Tomita.Shift where

import Data.List.NonEmpty (NonEmpty((:|)), sortWith)
import Data.List.NonEmpty qualified as NonEmpty
import Hanjiru.Tomita.Parse
import Hanjiru.Tomita.Stack

shift :: Int -> Int -> token -> ParseStack token a -> ParseStack token a
shift uniq state tok stack =
    push uniq state (Literal tok) stack
