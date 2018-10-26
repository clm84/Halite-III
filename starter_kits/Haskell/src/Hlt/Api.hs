module Hlt.Api where

import Prelude hiding (const)
import Control.Monad.Reader
import Data.Map.Strict ((!), elems, member)

import Hlt.Constants
import Hlt.Types

getConst :: GameEnv ConstantDict
getConst = const <$> ask

getBoolConst :: ConstantKey -> GameEnv Bool
getBoolConst key = getBool key <$> getConst

getIntConst :: ConstantKey -> GameEnv Int
getIntConst key = getInt key <$> getConst

getDoubleConst :: ConstantKey -> GameEnv Double
getDoubleConst key = getDouble key <$> getConst

getState :: GameEnv GameState
getState = state <$> ask

getTurn :: GameEnv Int
getTurn = turn <$> getState

getOwnId :: GameEnv PlayerId
getOwnId = pid <$> getOwnPlayer

getOwnPlayer :: GameEnv Player
getOwnPlayer = ownPlayer <$> getState

getOwnShipYard :: GameEnv Structure
getOwnShipYard = shipYard <$> getOwnPlayer

getOwnDropOffs :: GameEnv [Structure]
getOwnDropOffs = elems . dropOffs <$> getOwnPlayer

getOwnStructs :: GameEnv [Structure]
getOwnStructs = do
  shipYard <- getOwnShipYard
  dropOffs <- getOwnDropOffs
  return $ shipYard:dropOffs

getOwnShips :: GameEnv [Ship]
getOwnShips = elems . ships <$> getOwnPlayer

getAllPlayers :: GameEnv [Player]
getAllPlayers = elems . allPlayers <$> getState

getAllShipYards :: GameEnv [Structure]
getAllShipYards = map shipYard <$> getAllPlayers

getAllDropOffs :: GameEnv [Structure]
getAllDropOffs = concat . map (elems . dropOffs) <$> getAllPlayers

getAllStructs :: GameEnv [Structure]
getAllStructs = do
  shipYards <- getAllShipYards
  dropOffs <- getAllDropOffs
  return $ shipYards ++ dropOffs

getAllShips :: GameEnv [Ship]
getAllShips = concat . map (elems . ships) <$> getAllPlayers

getPlayer :: PlayerId -> GameEnv Player
getPlayer pid = do
  playerMap <- allPlayers <$> getState
  return $ playerMap ! pid

getShipYard :: PlayerId -> GameEnv Structure
getShipYard pid = shipYard <$> getPlayer pid

getDropOff :: PlayerId -> StructId -> GameEnv Structure
getDropOff pid did = do
  player <- getPlayer pid
  return $ dropOffs player ! did

hasShip :: PlayerId -> ShipId -> GameEnv Bool
hasShip pid sid = member sid . ships  <$> getPlayer pid

getShip :: PlayerId -> ShipId -> GameEnv Ship
getShip pid sid = do
  player <- getPlayer pid
  return $ ships player ! sid

getMap :: GameEnv GameMap
getMap = gameMap <$> getState

getCell :: Loc -> GameEnv Cell
getCell l = do
  cells <- cells <$> getMap
  return $ cells ! l
