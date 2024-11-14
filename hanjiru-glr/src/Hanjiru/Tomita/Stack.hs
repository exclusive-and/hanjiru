module Hanjiru.Tomita.Stack where

import Hanjiru.Token

data ParseHead a = ParseHead {
      -- Int
      headState  :: Int
    , headToken  :: Token
    , headParses :: [Parse a]
    }

data Parse a =
      Literal Token
    | Production Token Int [Parse a]
    deriving Show

data ParseStack a = ParseStack (ParseHead a) [ParseStack a]

top :: ParseStack a -> Int
top (ParseStack h _) = headState h

push :: Int -> Token -> ParseStack a -> ParseStack a
push state tok stack =
    ParseStack (ParseHead state tok [Literal tok]) [stack]

peek :: ParseStack a -> ParseHead a
peek (ParseStack h _) = h

popN :: Int -> ParseStack a -> [([ParseHead a], [ParseStack a])]
popN 0 stack                 = ([], [stack]) : []
popN 1 (ParseStack h []    ) = ([h], []) : []
popN n (ParseStack h stacks) =
    concatMap (map (first (a:)) . popN (n - 1)) stacks
