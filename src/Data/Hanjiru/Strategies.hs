module Data.Hanjiru.Strategies where

import Control.Monad.Trans.Writer

-- | Breadth-first work strategy.
--
-- This strategy performs the current worklist through to completion in one sweep. At the same
-- time, it collects the work to do on the next pass.
--
-- The strategy finishes when the next worklist is empty.

bfsM :: (Monoid a, Monad m) => (k -> WriterT [k] m a) -> [k] -> m a
bfsM f = go mempty
    where
        go acc [] = pure acc
        go acc xs = do
            (acc', ys) <- runWriterT (mconcat <$> mapM f xs)
            go (acc <> acc') ys