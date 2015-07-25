module Physics.ConstraintError where

import Control.Lens
import Linear.Metric
import Physics.Constraint

lensDifference :: Getting a s a -> (a -> a -> t) -> s -> s -> t
lensDifference l diff o o' = diff (o ^. l) (o' ^. l)

sqLinVelChange :: (Num a) => ErrorMetric a
sqLinVelChange = lensDifference physObjVel qd

linVelChange :: (Floating a) => ErrorMetric a
linVelChange = lensDifference physObjVel (\a b -> norm (a - b))

rotVelChange :: (Num a) => ErrorMetric a
rotVelChange = lensDifference physObjRotVel (\a b -> abs (a - b))
