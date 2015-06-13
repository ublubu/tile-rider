{-# LANGUAGE DataKinds #-}

module Physics.Constraint where

import Data.Vector
import Data.Maybe
import Linear.Affine
import Linear.Epsilon
import Linear.V
import Linear.V2
import Linear.V4
import Linear.Vector
import Linear.Matrix
import Linear.Metric
import Physics.Linear
import Physics.Geometry
import Physics.Transform

type MassInertia2 a = (a, a)
data PhysicalObj a = PhysicalObj { physObjVel :: V2 a
                                 , physObjRotVel :: a
                                 , physObjPos :: V2 a
                                 , physObjRotPos :: a
                                 , physObjHull :: ConvexHull a
                                 , physObjMass :: MassInertia2 a }

massM2 :: (Num a) => MassInertia2 a -> MassInertia2 a -> M66 a
massM2 (ma, ia) (mb, ib) = listToV [ listToV [ma, 0, 0, 0, 0, 0]
                                  , listToV [0, ma, 0, 0, 0, 0]
                                  , listToV [0, 0, ia, 0, 0, 0]
                                  , listToV [0, 0, 0, mb, 0, 0]
                                  , listToV [0, 0, 0, 0, mb, 0]
                                  , listToV [0, 0, 0, 0, 0, ib] ]

invMassM2 :: (Fractional a) => MassInertia2 a -> MassInertia2 a -> M66 a
invMassM2 (_ma, _ia) (_mb, _ib) = listToV [ listToV [ma, 0, 0, 0, 0, 0]
                                         , listToV [0, ma, 0, 0, 0, 0]
                                         , listToV [0, 0, ia, 0, 0, 0]
                                         , listToV [0, 0, 0, mb, 0, 0]
                                         , listToV [0, 0, 0, 0, mb, 0]
                                         , listToV [0, 0, 0, 0, 0, ib] ]
  where ma = 1 / _ma
        ia = 1 / _ia
        mb = 1 / _mb
        ib = 1 / _ib


physObjTransform :: (Floating a, Ord a) => PhysicalObj a -> WorldTransform a
physObjTransform obj = toTransform (physObjPos obj) (physObjRotPos obj)

physicsShape :: (Epsilon a, Floating a, Ord a) => PhysicalObj a -> ShapeInfo a
physicsShape obj = shapeInfo (LocalT (physObjTransform obj) (physObjHull obj))

velocity2 :: PhysicalObj a -> PhysicalObj a -> V6 a
velocity2 a b = (va `append2` wa) `join33` (vb `append2` wb)
  where va = physObjVel a
        vb = physObjVel b
        wa = physObjRotVel a
        wb = physObjRotVel b

lagrangian2 :: (Num a, Fractional a) => V6 a -> a -> PhysicalObj a -> PhysicalObj a -> a
lagrangian2 j b o1 o2 = (-(j `dot` v + b)) / mc
  where v = velocity2 o1 o2
        mc = effMassM2 j o1 o2

effMassM2 :: (Num a, Fractional a) => V6 a -> PhysicalObj a -> PhysicalObj a -> a
effMassM2 j a b = (j *! im) `dot` j
  where im = invMassM2 (physObjMass a) (physObjMass b)

constraintImpulse2 :: (Num a) => V6 a -> a -> V6 a
constraintImpulse2 j lagr = j ^* lagr

updateVelocity2_ :: (Num a) => V6 a -> M66 a -> V6 a -> V6 a
updateVelocity2_ v im pc = v + (im !* pc)
