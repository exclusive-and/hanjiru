module Data.Hanjiru.Graph where

import Control.Monad.ST
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Primitive.Array

-- | Strongly connected component.

data SCC a = Trivial a Vertex | Cycle a (NonEmpty Vertex)
    deriving (Eq, Show)

-- | \(O(V + E)\). Apply a function to each vertex, and combine results of reachable vertices
--   [[DeRemer, 1982](https://doi.org/10.1145/69622.357187)].

sccmap :: forall b a. Monoid b => (a -> b) -> Graph a -> [SCC b]
sccmap f (Graph g xs) =
    let
        (sccs, _stack) = runST $ do
            ns <- newArray (length g) 0
            ys <- newArray (length g) mempty

            sccfold ([], []) [0..(length g - 1)]
                `runReaderT` (ns, ys)
    in
        sccs
    where
        sccfold :: ([SCC b], [Vertex]) -> [Vertex] -> SccInfo s b
        sccfold = foldrM (\v s -> whenInteresting go s v)

        whenInteresting f s v = do
            (ns, _) <- ask
            n <- ns `readArray` v
            if n == 0
                then f s v
                else pure s
        
        go :: ([SCC b], [Vertex]) -> Vertex -> SccInfo s b
        go (sccs, stack) v = do
            (ns, ys) <- ask
            -- 1. Compute initial preorder number
            let depth = length stack + 1
            writeArray ns v depth
            -- 2. Recurse on adjacent vertices
            let ws = g `indexArray` v
            (sccs', stack') <- sccfold (sccs, v:stack) ws
            -- 3. Compute new preorder
            ns' <- traverse (readArray ns) ws
            let n' = foldr min depth ns'
            writeArray ns v n'
            -- 4. Compute immediate and combined results
            let y = f (xs `indexArray` v)
            ys' <- traverse (readArray ys) ws
            let y' = foldr (<>) y ys'
            writeArray ys v y'
            -- 5. Create a new SCC if one is detected
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

-- | The information used in the SCC-finding algorithm:
--
--    * Array of vertex preorder numbers.
--    * Array of partial results for each vertex.
--    * List of SCCs that have been found so far.
--    * Stack of vertices representing the path that the algorithm is currently exploring.

type SccInfo s a =
    ReaderT (MutableArray s Int, MutableArray s a)
            (ST s)
            ([SCC a], [Vertex])

-- | Directed graph.

data Graph a = Graph
    { outs  :: Array [Vertex]   -- ^ Vertices reached by outgoing edges.
    , nodes :: Array a          -- ^ Data nodes.
    }

-- | Vertex in a 'Graph'.

type Vertex = Int

-- | Construct a graph from adjacency lists.

fromAdjacencies :: [[Vertex]] -> Graph Vertex
fromAdjacencies adjs =
    let
        g = arrayFromList adjs
        xs = arrayFromList [0..length g]
    in
        Graph g xs