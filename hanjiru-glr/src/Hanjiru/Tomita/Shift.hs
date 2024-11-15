module Hanjiru.Tomita.Shift where

import Hanjiru.Token
import Hanjiru.Tomita.Parse
import Hanjiru.Tomita.Stack

import Data.List.NonEmpty (NonEmpty((:|)), sortWith)
import Data.List.NonEmpty qualified as NonEmpty

shift :: Int -> Int -> Token -> ParseStack a -> ParseStack a
shift uniq state tok stack =
    push uniq state (Literal tok) stack

merge :: [ParseStack a] -> [ParseStack a]
-- short circuit: no stacks --> nothing to merge
merge []     = []
merge stacks =
    let
        x :| xs = sortWith top (NonEmpty.fromList stacks)
    in
        go [] x xs
  where
    go merged s []     = s : merged
    go merged s (x:xs) =
        case tryCombine s x of
            Just s' -> go merged s' xs
            Nothing -> go (s : merged) x xs

    tryCombine (ParseStack uniq0 s xs) (ParseStack uniq1 t ys)
        | s == t    = Just (ParseStack (max uniq0 uniq1) s $ xs ++ ys)
        | otherwise = Nothing
