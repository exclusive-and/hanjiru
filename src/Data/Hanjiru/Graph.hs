module Data.Hanjiru.Graph where

import Control.Monad.ST
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Primitive.Array

-- | Strongly connected component.
data SCC a = Trivial a Vertex | Cycle a (NonEmpty Vertex)
    deriving (Eq, Show)

-- | \(O(V + E)\). Apply a function to each vertex, and combine the results of reachable
--   vertices. [Citation](https://doi.org/10.1145/69622.357187)
--
-- Reachability analysis necessarily computes SCCs, which are then combined into one value.
sccmap :: forall a b. Monoid b => (a -> b) -> Graph a -> [SCC b]
sccmap f (Graph g xs) =
    let
        (sccs, _stack) = runST $ do
            ns <- newArray (length g) 0
            ys <- newArray (length g) (error "impossible")

            sccfold ([], []) [0..(length g - 1)]
                `runReaderT` (ns, ys)
    in
        sccs
    where
        sccfold :: ([SCC b], [Vertex]) -> [Vertex] -> SccStep s b
        sccfold = foldrM (\v s -> whenInteresting go s v)

        whenInteresting f s v = do
            (ns, _) <- ask
            n <- ns `readArray` v
            if n == 0
                then f s v
                else pure s
        
        go :: ([SCC b], [Vertex]) -> Vertex -> SccStep s b
        go (sccs, stack) v = do
            (ns, ys) <- ask
            -- 1. Compute preorder and immediate result
            let depth = length stack + 1
            writeArray ns v depth
            let y = f (xs `indexArray` v)
            writeArray ys v y
            -- 2. Recurse on adjacent vertices
            let ws = g `indexArray` v
            (sccs', stack') <- sccfold (sccs, v:stack) ws
            -- 3. Compute new preorder and combined result
            ns' <- traverse (readArray ns) ws
            let n' = foldr min depth ns'
            writeArray ns v n'
            ys' <- traverse (readArray ys) ws
            let y' = foldr (<>) y ys'
            writeArray ys v y'
            -- 4. Create a new SCC if one is detected
            if n' == depth
                then consScc [] v (sccs', stack')
                else pure (sccs', stack')
        
        consScc scc v (sccs, []) = pure (sccs, [])
        consScc scc v (sccs, x:stack) = do
            (ns, ys) <- ask
            writeArray ns x maxBound
            if v == x
                then do
                    y <- ys `readArray` x
                    if  | not (x `elem` g `indexArray` x)
                        , null scc  -> pure (Trivial y x : sccs, stack)
                        | otherwise -> pure (Cycle y (x :| scc) : sccs, stack)
                else consScc (x:scc) v (sccs, stack)

type SccStep s a =
    ReaderT (MutableArray s Int, MutableArray s a)
            (ST s)
            ([SCC a], [Vertex])

type Vertex = Int

data Graph a = Graph
    { outs  :: Array [Vertex]
    , nodes :: Array a
    }