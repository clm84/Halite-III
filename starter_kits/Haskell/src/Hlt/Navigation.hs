{-# LANGUAGE LambdaCase #-}

module Hlt.Navigation where


import qualified Data.List as L
import Data.Map (Map, (!), elems)
import Data.Maybe

import Hlt.Utils
import Hlt.Types


toOffset :: Direction -> Loc
toOffset = \case
  North -> (0, -1)
  East  -> (1, 0)
  South -> (0, 1)
  West  -> (-1, 0)

dirOffset :: Loc -> Direction -> Loc
dirOffset l d = l + toOffset d

dirOffsets :: Loc -> [Direction] -> Loc
dirOffsets = foldl dirOffset

cellAt :: GameMap -> Loc -> Cell
cellAt gm l = cells gm ! l

cellUnder :: Located a => GameMap -> a -> Cell
cellUnder gm entity = cells gm ! loc entity

wrap :: GameMap -> Loc -> Loc
wrap gm (x, y) = (mod x w, mod y h)
  where (w, h) = shape gm

shortPath :: (Located a, Located b) => GameMap -> a -> b -> Loc
shortPath gm a b = (aux w u, aux h v)
  where
    (w, h) = shape gm
    (u, v) = loc b - loc a
    aux l x
      | abs x > div l 2 = signum x * x - l
      | otherwise = x

distance :: (Located a, Located b) => GameMap -> a -> b -> Int
distance gm a b = norm1 $ shortPath gm a b

targetDir :: (Located a, Located b) => GameMap -> a -> b -> [Direction]
targetDir gm a b = catMaybes [northSouth, eastWest]
  where
    northSouth = caseSign v (Just South, Nothing, Just North)
    eastWest   = caseSign u (Just East, Nothing, Just West)
    (u, v)     = shortPath gm a b

isFreeDir :: Located a => GameMap -> a -> Direction -> Bool
isFreeDir gm entity = not . isOccupied . cellAt gm . wrap gm . dirOffset (loc entity)

naiveNavigate :: (Located a, Located b) => GameMap -> a -> b -> Maybe Direction
naiveNavigate gm a = L.find (isFreeDir gm a) . (targetDir gm a)

surroundings :: Located a => GameMap -> a -> [Cell]
surroundings gm entity = map (cellAt gm . wrap gm . dirOffset (loc entity)) cardinals
