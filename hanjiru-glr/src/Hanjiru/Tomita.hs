module Hanjiru.Tomita where

import Hanjiru.Token
import Hanjiru.Tomita.Parse
import Hanjiru.Tomita.Reduce
import Hanjiru.Tomita.Shift
import Hanjiru.Tomita.Stack

import Data.List (sortOn)

parse :: forall a. Eq a => ActionTable -> GotoTable -> [Token] -> ([ParseResult a], [ParseStack a])
parse action goto = foldl (tomita action goto) ([], [ParseStack 0 0 []])

tomita :: forall a. Eq a => ActionTable -> GotoTable -> ([ParseResult a], [ParseStack a]) -> Token -> ([ParseResult a], [ParseStack a])
tomita action goto (results, stacks) tok =
        let
            (results', stacks') = go [] tok stacks
        in
            (results <> results', stacks')
    where
    go _ tok stacks =
        let
            (results', stacks') = reduceAll [] tok stacks
        in
            (results', shiftAll tok stacks')
    
    reduceAll results _ [] = (results, [])
    reduceAll results tok (stack:stacks) =
        case action (top stack) tok of
            Accept -> reduceAll (ParseOK (peek stack) : results) tok stacks
            Error  -> reduceAll results tok stacks
            Reduce rs -> goReduceAll rs
            Shift  rs state -> let (results', stacks') = goReduceAll rs in (results', (push 0 state tok stack):stacks')
        where
        goReduceAll rs =
            let
                reds = concat [ reduce r stack | r <- rs ]
                stacks' = foldl (\stk (p, s) -> pack (goto (top s) (symbol p)) p s stk) stacks reds
            in
                reduceAll results tok stacks'
    
    shiftAll tok [] = []
    shiftAll tok stacks =
        merge stacks

data ParseResult a =
      ParseOK [Parse a]
    | ParseError (ParseStack a)
    deriving Show

data Action =
      Accept
    | Error
    | Reduce [Reduction]
    | Shift  [Reduction] Int

{- *** Action tables *** -}

type ActionTable    = Int -> Token -> Action
type GotoTable      = Int -> Token -> Int