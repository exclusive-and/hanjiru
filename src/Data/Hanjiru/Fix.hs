module Data.Hanjiru.Fix where

import Control.Monad.Trans.Writer
import Data.Monoid (Any (..))

type Changes = Writer Any

-- | Record that something changed.
changed :: a -> Changes a
changed x = tell (Any True) >> pure x

-- | Apply a function to a value if the value has a recorded change.
apChanges :: (a -> a) -> Changes a -> a
apChanges f cx =
    let
        (y, hasChanges) = runWriter cx
    in
        if getAny hasChanges then f y else y

-- | Repeatedly apply a function to a value until it does not record any more changes.
saturate :: (a -> Changes a) -> a -> a
saturate f = go
    where
        go x = apChanges go (f x)