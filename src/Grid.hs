{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Grid where

import qualified Graphics.UI.SDL.Types as SDL.T

data Tile a = Tile
  { tileRight :: Maybe (Tile a)
  , tileDown :: Maybe (Tile a)
  , tileContents :: a
  }

data TileCursor a = TileCursor
  { cursorTile :: Tile a
  , cursorLeft :: Maybe (TileCursor a)
  , cursorUp :: Maybe (TileCursor a)
  }

cursorRight :: TileCursor a -> Maybe (TileCursor a)
cursorRight c = case tileRight (cursorTile c) of
  Nothing -> Nothing
  Just t -> Just TileCursor { cursorTile = t
                            , cursorLeft = Just c
                            , cursorUp = cursorRight =<< cursorUp c }

cursorDown :: TileCursor a -> Maybe (TileCursor a)
cursorDown c = case tileDown (cursorTile c) of
  Nothing -> Nothing
  Just t -> Just TileCursor { cursorTile = t
                            , cursorLeft = cursorDown =<< cursorLeft c
                            , cursorUp = Just c }

moveRight :: (Int, Int) -> (Int, Int)
moveRight (x, y) = (x + 1, y)

moveLeft :: (Int, Int) -> (Int, Int)
moveLeft (x, y) = (x - 1, y)

moveDown :: (Int, Int) -> (Int, Int)
moveDown (x, y) = (x, y + 1)

moveUp :: (Int, Int) -> (Int, Int)
moveUp (x, y) = (x, y - 1)

instance Functor Tile where
  fmap f t = Tile { tileRight = f' =<< tileRight t
                  , tileDown = f' =<< tileDown t
                  , tileContents = f (tileContents t)}
    where f' = return . fmap f

instance Functor TileCursor where
  fmap f c = TileCursor { cursorTile = fmap f tile
                        , cursorLeft = f' =<< cl
                        , cursorUp = f' =<< cu }
    where tile = cursorTile c
          cl = cursorLeft c
          cu = cursorUp c
          f' = return . fmap f

instance Grid (Tile a) (Tile b) a b where
  gridMap f pos t = Tile { tileRight = mapTile tileRight moveRight
                         , tileDown = mapTile tileDown moveDown
                         , tileContents = f pos (tileContents t)}
    where mapTile getTile getPos = (return . (gridMap f $ getPos pos)) =<< getTile t

instance Grid (TileCursor a) (TileCursor b) a b where
  gridMap f pos c = TileCursor { cursorTile = gridMap f pos (cursorTile c)
                               , cursorLeft = mapCursor cursorLeft moveLeft
                               , cursorUp = mapCursor cursorLeft moveUp }
    where mapCursor getCursor getPos = (return . (gridMap f $ getPos pos)) =<< getCursor c

class Grid g h a b | g -> a, h -> b where
  gridMap :: ((Int, Int) -> a -> b) -> (Int, Int) -> g -> h

