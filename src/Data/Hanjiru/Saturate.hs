module Data.Hanjiru.Saturate where

import Control.Monad.Trans.Writer
import Data.Monoid

type Change = Writer Any

changed :: a -> Change a
changed x = tell (Any True) >> pure x

saturate :: (a -> Change a) -> a -> a
saturate f = loop where
    loop x =
        case runWriter (f x) of
            (y, Any True) -> loop y
            (y, _       ) -> y