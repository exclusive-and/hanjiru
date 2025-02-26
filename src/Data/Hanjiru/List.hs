{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Data.Hanjiru.List where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid (Monoid (..))

nonEmpty :: (a -> [a] -> b) -> NonEmpty a -> b
nonEmpty k (x :| xs) = k x xs

list :: b -> (a -> [a] -> b) -> [a] -> b
list nil cons []     = nil
list nil cons (x:xs) = cons x xs

mlist :: Monoid m => (a -> [a] -> m) -> [a] -> m
mlist = list mempty