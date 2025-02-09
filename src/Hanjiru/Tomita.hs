module Hanjiru.Tomita where

import Control.Monad (foldM)
import Control.Monad.Writer
import Data.List (sort)
import Hanjiru.Tables
import Hanjiru.Tomita.Parse
import Hanjiru.Tomita.Reduce (Reduced(..))
import Hanjiru.Tomita.Reduce qualified as Hanjiru
import Hanjiru.Tomita.Shift  qualified as Hanjiru
import Hanjiru.Tomita.Stack

parse :: forall a token.
       (Ord token, Eq a)
    => ActionTable token
    -> GotoTable token
    -> [token]
    -> [ParseResult token a]
parse action goto = execWriter . foldM tomita [ParseStack 0 0 []]
    where
    tomita ::
           [ParseStack token a]
        -> token
        -> Writer [ParseResult token a] [ParseStack token a]
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

data ParseResult token a =
      ParseOk [Parse token a]
    | ParseError (ParseStack token a)
    deriving Show

{- *** Action tables *** -}
