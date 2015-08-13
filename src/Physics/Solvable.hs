{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RankNTypes #-}

module Physics.Solvable where

import Physics.WorldSolver

-- w: world
-- k: key
-- s: solution
-- t: target
-- c: cache
-- x: arbitrary arg
class Solvable s t c x where
  slnApply :: s -> c -> t -> (c, t)
  slnGen :: x -> t -> t -> s
  slnEmptyCache :: c

init :: (Solvable s t c x) => WSGen w k t x c
init ks l w x c = ]
