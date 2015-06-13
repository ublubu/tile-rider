module Physics.World where

import Physics.Constraint

data World  a = World { worldObjs :: [PhysicalObj a] }


