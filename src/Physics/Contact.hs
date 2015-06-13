{-# LANGUAGE DataKinds #-}

module Physics.Contact where

import Control.Lens
import Data.Either
import Linear.Affine
import Linear.Epsilon
import Linear.V
import Linear.V2
import Linear.Vector
import Linear.Matrix
import qualified Physics.Geometry as G
import Physics.Linear
import Physics.Constraint
import Physics.Transform
import Utils.Utils

data Contact a = Contact { contactA :: PhysicalObj a
                         , contactB :: PhysicalObj a
                         , contactPoint :: V2 a
                         , contactNormal :: V2 a }

generateContact :: (Epsilon a, Floating a, Ord a) => PhysicalObj a -> PhysicalObj a -> [Contact a]
generateContact a b = case mc of Nothing -> []
                                 Just c -> eitherFlip f c a b
  where sa = physicsShape a
        sb = physicsShape b
        mc = G.contact sa sb
        f c' a' b' = fmap g ps
          where ps = G.flattenContactPoints cc
                cc = iExtract c'
                n = G.contactNormal cc
                g p = Contact { contactA = a'
                              , contactB = b'
                              , contactPoint = view _Point p
                              , contactNormal = n }

jacobian :: (Num a) => Contact a -> V6 a
jacobian (Contact a b p n) = ja `join33` jb
  where ja = (-n) `append2` ((xa - p) `cross22` n)
        jb = n `append2` ((p - xb) `cross22` n)
        xa = physObjPos a
        xb = physObjPos b

velocity :: Contact a -> V6 a
velocity (Contact a b _ _) = (va `append2` wa) `join33` (vb `append2` wb)
  where va = physObjVel a
        vb = physObjVel b
        wa = physObjRotVel a
        wb = physObjRotVel b

