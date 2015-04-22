{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Grid where

import qualified Graphics.UI.SDL.Types as SDL.T
import Control.Applicative

data Tile a = Tile a a a a

class Orientable o where
  orientableRotate :: o a -> o a

instance Orientable Tile where
  orientableRotate (Tile a b c d) = Tile b c d a

data GridOrientation = GridRight | GridDown | GridLeft | GridUp deriving Eq

data Oriented a = Oriented GridOrientation a

rotate :: Oriented a -> Oriented a
rotate (Oriented GridRight x) = Oriented GridDown x
rotate (Oriented GridDown x) = Oriented GridLeft x
rotate (Oriented GridLeft x) = Oriented GridUp x
rotate (Oriented GridUp x) = Oriented GridRight x

rotateTo :: GridOrientation -> Oriented a -> Oriented a
rotateTo o a@(Oriented o' _) = if o == o' then a else rotateTo o (rotate a)

oriAp :: Orientable o => Oriented (o a -> b) -> Oriented (o a) -> Oriented b
oriAp a@(Oriented ori f) b@(Oriented ori' x) =
  if ori == ori' then Oriented ori $ f x
  else oriAp a (rotate b)

data Corner a = Corner (Maybe (Corner a)) (Maybe (Corner a)) a

instance Functor Corner where
  fmap f (Corner a b x) = Corner a' b' (f x)
    where a' = f' a
          b' = f' b
          f' = fmap (fmap f)

flipCorner :: Corner a -> Corner a
flipCorner (Corner a b x) = Corner (fmap flipCorner b) (fmap flipCorner a) x

ahead :: Corner a -> Maybe (Corner a)
ahead (Corner a _ _) = a

adjacent :: Corner a -> Maybe (Corner a)
adjacent (Corner _ a _) = a

newtype CornerCursor a = CornerCursor { cursorTile :: Tile (Corner a) }

instance Functor CornerCursor where
  fmap f c = CornerCursor { cursorTile = Tile (fmap f w) (fmap f x) (fmap f y) (fmap f z) }
    where (Tile w x y z) = cursorTile c

extendAhead :: Corner a -> Corner a -> Maybe (Corner a)
extendAhead w x = fmap (\(Corner _ _ contents) -> Corner c' (Just x) contents) w'
  where c  = ahead x
        d  = adjacent w
        w' = ahead w
        c' = do
          dd <- d
          cc <- c
          extendAhead dd cc

extendAdjacent :: Corner a -> Corner a -> Maybe (Corner a)
extendAdjacent z y = extendAhead (flipCorner z) (flipCorner y)

advance :: CornerCursor a -> Maybe (CornerCursor a)
advance cursor = do
  w' <- ahead w
  x' <- extendAhead w x
  y' <- extendAdjacent z y
  z' <- adjacent z
  return CornerCursor { cursorTile = Tile w' x' y' z'}
    where (Tile w x y z) = cursorTile cursor



