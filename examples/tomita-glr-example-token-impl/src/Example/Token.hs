module Example.Token where

data Token =
      TokStart
    | TokEnd
    | Noun String
    | Verb String
    | Det  String
    | Prep String
    | S
    | NP
    | VP
    | PP
    deriving (Eq, Show)