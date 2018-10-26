module MyBot where

import qualified Data.List as L
import Data.Map.Strict ((!), keys)
import Data.Maybe

import Hlt.Utils
import Hlt.Constants
import Hlt.Types
import Hlt.Api
import Hlt.Navigation
import Hlt.Game


play :: GameEnv [Command]
play = do
  spawn <- maybeToList <$> computeSpawn
  moves <- computeMoves
  return $ spawn ++ moves

computeSpawn :: GameEnv (Maybe Command)
computeSpawn = do
  shipCost <- getIntConst ShipCost
  turn <- getTurn
  gm <- getMap
  decideSpawn shipCost turn gm <$> getOwnPlayer

computeMoves :: GameEnv [Command]
computeMoves = do
  maxHalite <- getIntConst MaxHalite
  turn <- getTurn
  gm <- getMap
  player <- getOwnPlayer
  map (selectMove maxHalite turn gm player) <$> getOwnShips

decideSpawn :: Int -> Int -> GameMap -> Player -> Maybe Command
decideSpawn shipCost turn gm player
  | turn <= 200 && halite player > shipCost && isShipYardFree player = Just Spawn
  | otherwise = Nothing
  where isShipYardFree = not . isOccupied . cellUnder gm . shipYard

selectMove :: Int -> Int -> GameMap -> Player -> Ship -> Command
selectMove maxHalite turn gm player ship
  | halite (cellUnder gm ship) < div maxHalite 10 = pseudoRndDir turn gm ship
  | otherwise = Collect ship

pseudoRndDir :: Int -> GameMap -> Ship -> Command
pseudoRndDir turn gm ship = case dir of
  Just d -> Move ship d
  Nothing -> Collect ship
  where
    dir = L.find (isFreeDir gm ship) choices
    choices = rotate (mod (turn + (toInt . sid $ ship)) 4) cardinals


main :: IO ()
main = runGame "MyBot" play
