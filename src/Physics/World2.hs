{-# LANGUAGE TemplateHaskell #-}

module Physics.World2 where

import qualified Data.IntMap.Strict as IM
import Control.Lens
import Physics.Constraint

data World a = World (IM.IntMap a) IM.Key deriving Show

addObj :: a -> World a -> World a
addObj o (World im n) = World (IM.insert n o im) (n + 1)

data WorldObj n a = WorldObj { _woPhysObj :: PhysicalObj n
                             , _woExternals :: [External n]
                             , _woContext :: a }
makeLenses ''WorldObj

instance Functor (WorldObj n) where
  fmap f o = o & woContext %~ f


