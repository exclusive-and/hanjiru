module Hanjiru.Tomita where

import Hanjiru.Token
import Hanjiru.Tomita.Parse
import Hanjiru.Tomita.Reduce (Reduction, Reduced(..))
import Hanjiru.Tomita.Reduce qualified as Hanjiru
import Hanjiru.Tomita.Shift  qualified as Hanjiru
import Hanjiru.Tomita.Stack

import Control.Monad.Writer
import Data.List (sort)

parse :: forall a. Eq a => ActionTable -> GotoTable -> [Token] -> [ParseResult a]
parse action goto = execWriter . foldM tomita [ParseStack 0 0 []]
    where
    tomita :: [ParseStack a] -> Token -> Writer [ParseResult a] [ParseStack a]
    tomita [] _tok            = pure []
    tomita (stack:stacks) tok =
        case action (top stack) tok of
            Accept -> accept
            Error  -> reject
            Reduce rs       -> reduce rs
            Shift  rs state -> shift rs state
        where
        accept = tell [ParseOk (peek stack)] >> tomita stacks tok

        reject = tomita stacks tok

        reduce rs =
            let
                reduced = concatMap (Hanjiru.reduce stack) rs
                stacks' = foldr Hanjiru.pack stacks (map nextState reduced)
            in
                tomita stacks' tok

        shift rs state =
            let
                stack' = Hanjiru.shift 0 state tok stack
            in
                sort . Hanjiru.merge . (stack':) <$> reduce rs
    
    nextState (Reduced state tok parse', stack) =
        (Reduced (goto state tok) tok parse', stack)

data ParseResult a =
      ParseOk [Parse a]
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