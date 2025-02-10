module Data.Hanjiru.GSS where

import Data.Function (on)
import Data.List (insertBy)
import Data.List.NonEmpty (NonEmpty ((:|)), sortWith)
import Data.List.NonEmpty qualified as NE

-- | Tomita's graph-structured stack.

data GSS s a = GSS Int s [(a, GSS s a)]
    deriving Eq

push :: s -> a -> GSS s a -> GSS s a
push s a g@(GSS h _ _) = GSS (h + 1) s [(a, g)]

gss :: (s -> [a] -> b) -> Int -> GSS s a -> [(b, GSS s a)]
gss f = go []
    where
        go xs 0 g@(GSS _ s _ ) = [(f s xs, g)]
        go xs n   (GSS _ _ ts) = concatMap (\(a, g') -> go (a:xs) (n - 1) g') ts

height :: GSS s a -> Int
height (GSS h _ _) = h

top :: GSS s a -> s
top (GSS _ s _) = s

peek :: GSS s a -> [a]
peek (GSS _ _ ts) = map fst ts

merge :: Ord s => [GSS s a] -> [GSS s a]
merge [] = []
merge gs =
    let
        x :| xs = sortWith top (NE.fromList gs)
    in
        go [] x xs
    where
        go merged s []     = s : merged
        go merged s (x:xs) =
            case tryCombine s x of
                Just s' -> go merged s' xs
                Nothing -> go (s : merged) x xs

        tryCombine (GSS h0 s xs) (GSS h1 t ys)
            | s == t    = Just (GSS (max h0 h1) s $ xs ++ ys)
            | otherwise = Nothing

pack :: (Monoid a, Eq a, Ord s) => s -> a -> GSS s a -> [GSS s a] -> [GSS s a]
pack s a g = go []
    where
        go xs [] = insertBy (compare `on` height) (push s a g) (reverse xs)
        go xs (candidate:stacks) =
            case tryPack candidate of
                Just packed -> reverse xs ++ (packed:stacks)
                Nothing     -> go (candidate:xs) stacks
    
        tryPack (GSS h s' [(a', g')]) | s == s', g == g' = Just (push s (a <> a') g)
        tryPack _ = Nothing