{-# LANGUAGE LambdaCase #-}

module Example.Actions where

import Hanjiru.Tomita
import Hanjiru.Tomita.Reduce

import Example.Token

import Debug.Trace

actions :: ActionTable
actions 0 = \case
    Det _   -> Shift [] 3
    Noun _  -> Shift [] 4
    _       -> Error
actions 1 = \case
    Prep _  -> Shift [] 6
    TokEnd  -> Accept
    _       -> Error
actions 2 = \case
    Verb _  -> Shift [] 7
    Prep _  -> Shift [] 6
    _       -> Error
actions 3 = \case
    Noun _  -> Shift [] 10
    _       -> Error
actions 4 = \case
    Verb _  -> Reduce [MkReduction NP 1]
    Prep _  -> Reduce [MkReduction NP 1]
    TokEnd  -> Reduce [MkReduction NP 1]
    _       -> Error
actions 5 = \case
    Prep _  -> Reduce [MkReduction S 2]
    TokEnd  -> Reduce [MkReduction S 2]
    _       -> Error
actions 6 = \case
    Det _   -> Shift [] 3
    Noun _  -> Shift [] 4
    _       -> Error
actions 7 = \case
    Det _   -> Shift [] 3
    Noun _  -> Shift [] 4
    _       -> Error
actions 8 = \case
    Prep _  -> Reduce [MkReduction S 2]
    TokEnd  -> Reduce [MkReduction S 2]
    _       -> Error
actions 9 = \case
    Verb _  -> Reduce [MkReduction NP 2]
    Prep _  -> Reduce [MkReduction NP 2]
    TokEnd  -> Reduce [MkReduction NP 2]
    _       -> Error
actions 10 = \case
    Verb _  -> Reduce [MkReduction NP 2]
    Prep _  -> Reduce [MkReduction NP 2]
    TokEnd  -> Reduce [MkReduction NP 2]
    _       -> Error
actions 11 = \case
    Verb _  -> Reduce [MkReduction PP 2]
    Prep _  -> Shift  [MkReduction PP 2] 6
    TokEnd  -> Reduce [MkReduction PP 2]
    _       -> Error
actions 12 = \case
    Prep _  -> Shift  [MkReduction VP 2] 6
    TokEnd  -> Reduce [MkReduction VP 2]
    _       -> Error
actions _ = const Error

gotos :: GotoTable
gotos  0 NP = 2
gotos  6 NP = 11
gotos  7 NP = 12
gotos  1 PP = 5
gotos  2 PP = 9
gotos 11 PP = 9
gotos 12 PP = 9
gotos  2 VP = 8
gotos  0  S = 1
gotos  n  t = trace ("invalid goto: " ++ "state = " ++ show n ++ " and tok = " ++ show t) (-1)