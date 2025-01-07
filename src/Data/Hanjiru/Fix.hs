module Data.Hanjiru.Fix where

import Control.Monad.Trans.Writer
import Data.Monoid (Any (..))

-- | Keep a qualitative record of whether something changed.
--
-- Change indirectly measures convergence: a function has converged when applying it again
-- does not result in any change to its input.
type Changes = Writer Any

-- | Record that something changed.
changed :: a -> Changes a
changed x = tell (Any True) >> pure x

-- | Apply a function to a value if it hasn't converged.
apChanges :: (a -> a) -> Changes a -> a
apChanges f cx =
    let
        (y, hasChanges) = runWriter cx
    in
        if getAny hasChanges then f y else y

-- | Repeatedly apply a function to a value until it converges.
saturate :: (a -> Changes a) -> a -> a
saturate f = go
    where
        go x = apChanges go (f x)