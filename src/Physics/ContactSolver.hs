{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes, TemplateHaskell #-}

module Physics.ContactSolver where

import Control.Lens
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Physics.Constraint
import Physics.PairMap
import Physics.World hiding (solveOne, solveMany)
import Physics.WorldSolver
import Utils.Utils

data ConstraintGen n a = ConstraintGen { _constraintNonPen :: Constraint' n }
makeLenses ''ConstraintGen
type ConstraintGen' n a = n -> (a, a) -> [(Key, ConstraintGen n a)]

type SolutionCache n = n
type Cache n a = PairMap (SolutionCache n, ConstraintGen n a)
type State n a = (n, PairMap (Cache n a))
type SolutionProcessor n = SolutionCache n -> ConstraintResult n -> (SolutionCache n, ConstraintResult n)
type WorldLens2 k w a = WorldLens k w (a, a)

emptyState :: (Num n) => State n a
emptyState = (0, IM.empty)

init :: (Physical a n, Num n) => SolutionCache n -> ConstraintGen' n a -> WSGen (World a) Key (a, a) n (State n a)
init cache0 g ks l w x s0 = foldl f (x, IM.empty) ks
  where f s k = initOne cache0 g k l w x s0 s

initOne :: (Physical a n, Num n) => SolutionCache n -> ConstraintGen' n a -> Key -> WorldLens2 Key (World a) a -> World a -> n -> State n a -> State n a -> State n a
initOne cache0 g k l w x s0 s = s & _2 %~ insertPair k cache
  where ab = fromJust $ w ^? l k
        cs' = g x ab
        caches0 = s0 ^. _2
        cache = initCache cache0 (lookupPair k caches0) cs'

initCache :: (Num n) => SolutionCache n -> Maybe (Cache n a) -> [(Key, ConstraintGen n a)] -> Cache n a
initCache cache0 (Just cache) cs' = foldl f IM.empty cs'
  where f cache' (k, c') = insertPair k (sln', c') cache'
          where sln' = case lookupPair k cache of
                  Just (sln, _) -> sln
                  Nothing -> cache0
initCache cache0 Nothing cs' = foldl f IM.empty cs'
  where f cache' (k, c') = insertPair k (cache0, c') cache'

improve :: (Physical a n, Fractional n) => SolutionProcessor n -> WSFunc (World a) Key (a, a) (State n a)
improve sp l w s = (w', s')
  where (w', s') = foldl f (w, s) ks
        f (w0, s0) k = improveOne sp k l w0 s0
        ks = keys (snd s)

improveOne :: (Physical a n, Fractional n) => SolutionProcessor n -> Key -> WorldLens2 Key (World a) a -> World a -> State n a -> (World a, State n a)
improveOne sp k l w s = (w & l k .~ ab', s & _2 %~ insertPair k cache')
  where cache = fromJust $ lookupPair k (snd s)
        ab = fromJust $ w ^? l k
        (ab', cache') = solveMany sp (keys cache) cache ab

solveMany :: (Physical a n, Fractional n) => SolutionProcessor n -> [Key] -> Cache n a -> (a, a) -> ((a, a), Cache n a)
solveMany sp ks cache ab = foldl f (ab, cache) ks
  where f (ab0, cache0) k = (ab', insertPair k (sln', c') cache0)
          where (ab', sln') = solveOne sp k c' ab0 sln0
                (sln0, c') = fromJust $ lookupPair k cache0

solveOne :: (Physical a n, Fractional n) => SolutionProcessor n -> Key -> ConstraintGen n a -> (a, a) -> SolutionCache n -> ((a, a), SolutionCache n)
solveOne sp k c' ab sln = (ab', sln')
  where ab' = applyConstraintResult cr' ab
        cr = constraintResult (c' ^. constraintNonPen) ab
        (sln', cr') = sp sln cr
