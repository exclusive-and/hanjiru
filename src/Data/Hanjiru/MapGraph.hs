module Data.Hanjiru.MapGraph where

import Control.Arrow
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Hanjiru.Graph (Vertex)
import Data.Hanjiru.Strategies
import Data.Map (Map)
import Data.Map qualified as Map

-- | The context monad for 'identify'.

type Identify a = WriterT [(a, Int)] (State (Int, Map a Int))

-- | Identify a value with an 'Int' given a context.

identify :: Ord a => a -> Identify a Int
identify x = do
    (freshId, idMap) <- lift get
    case Map.lookup x idMap of
        Just oldId -> pure oldId
        Nothing -> do
            tell [(x, freshId)]
            lift $ put $ (freshId + 1, Map.insert x freshId idMap)
            pure freshId

-- | A directed graph with labelled edges, backed by 'Map'.

data MapGraph e v =
        MapGraph    { edges :: Map (Vertex, e) Vertex
                    , nodes :: Map Vertex v
                    }

instance Ord e => Semigroup (MapGraph e v) where
    MapGraph es0 xs0 <> MapGraph es1 xs1 = MapGraph (es0 <> es1) (xs0 <> xs1)

instance Ord e => Monoid (MapGraph e v) where
    mempty = MapGraph mempty mempty

-- | Unfold a 'MapGraph' from an initial vertex and a successor function.

unfold :: forall v e. (Ord v, Ord e) => (v -> [(e, v)]) -> v -> MapGraph e v
unfold f start = (bfsM visit =<< initial) `evalState` (0, mempty)
    where
        visit :: (v, Int) -> Identify v (MapGraph e v)
        visit (x, v) = do
            ws <- traverse (go v) (f x)
            pure $ MapGraph (Map.fromList ws) (Map.singleton v x)

        go :: Vertex -> (e, v) -> Identify v ((Vertex, e), Vertex)
        go v (e, w) = do
            w' <- identify w
            pure ((v, e), w')
        
        initial = execWriterT $ identify start