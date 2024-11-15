module Hanjiru.Tomita where

import Hanjiru.Token
import Hanjiru.Tomita.Parse
import Hanjiru.Tomita.Reduce (Reduction)
import Hanjiru.Tomita.Reduce qualified as Hanjiru
import Hanjiru.Tomita.Shift  qualified as Hanjiru
import Hanjiru.Tomita.Stack

import Control.Monad.Writer

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
                reduced = concatMap (reduceStep stack) rs
                stacks' = foldl packStep stacks reduced
            in
                tomita stacks' tok

        shift rs state =
            let
                stack' = Hanjiru.shift 0 state tok stack
            in
                Hanjiru.merge . (stack':) <$> reduce rs
    
    reduceStep :: ParseStack a -> Reduction -> [(Int, Parse a, ParseStack a)]
    reduceStep stack reduction =
        let
            next (parse', stack') = (state', parse', stack')
                where state' = goto (top stack') (symbol parse')
        in
            map next (Hanjiru.reduce reduction stack)
    
    packStep :: [ParseStack a] -> (Int, Parse a, ParseStack a) -> [ParseStack a]
    packStep stacks (state', parse', stack') =
        Hanjiru.pack state' parse' stack' stacks

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