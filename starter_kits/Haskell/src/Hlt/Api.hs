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
getOwnId = ownId <$> getState

getOwnPlayer :: GameEnv Player
getOwnPlayer = do
  players <- players <$> getState
  ownId <- getOwnId
  return $ players ! ownId

getOwnShipYard :: GameEnv Structure
getOwnShipYard = shipYard <$> getOwnPlayer

getOwnDropOffs :: GameEnv [Structure]
getOwnDropOffs = elems . dropOffs <$> getOwnPlayer

getOwnShips :: GameEnv [Ship]
getOwnShips = elems . ships <$> getOwnPlayer

getAllPlayers :: GameEnv [Player]
getAllPlayers = elems . players <$> getState

getAllShipYards :: GameEnv [Structure]
getAllShipYards = map shipYard <$> getAllPlayers

getAllDropOffs :: GameEnv [Structure]
getAllDropOffs = concat . map (elems . dropOffs) <$> getAllPlayers

getAllShips :: GameEnv [Ship]
getAllShips = concat . map (elems . ships) <$> getAllPlayers

getPlayer :: PlayerId -> GameEnv Player
getPlayer pid = do
  playerMap <- players <$> getState
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
