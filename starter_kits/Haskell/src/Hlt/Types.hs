{-# LANGUAGE LambdaCase #-}

module Hlt.Types where


import Control.Monad.Reader
import qualified Data.List as L
import Data.Map (Map, (!), elems)
import Data.Maybe

import Hlt.Utils
import Hlt.Constants


type Shape     = (Int, Int)
type Loc       = (Int, Int)
type PlayerSet = Map PlayerId Player
type GameEnv a = Reader GameRec a


data Direction = North | East | South | West

data Command = Spawn
             | Construct Ship
             | Collect Ship
             | Move Ship Direction

data PlayerId  = PlayerId Int deriving (Eq, Ord, Show)
data ShipId    = ShipId Int deriving (Eq, Ord, Show)
data StructId  = StructId Int deriving (Eq, Ord, Show)

data Ship = Ship { sid :: ShipId
                 , shipPid :: PlayerId
                 , shipLoc :: Loc
                 , shipHalite :: Int
                 , isFull :: Bool
                 } deriving Show

data Structure = Structure { structId :: StructId
                           , structPid :: PlayerId
                           , structLoc :: Loc
                           } deriving Show

data Player = Player { pid :: PlayerId
                     , playerHalite :: Int
                     , shipYard :: Structure
                     , dropOffs :: Map StructId Structure
                     , ships :: Map ShipId Ship
                     } deriving Show

data Cell = Cell { cellLoc :: Loc
                 , cellHalite :: Int
                 , ship :: Maybe Ship
                 , struct :: Maybe Structure
                 } deriving Show

data GameMap = GameMap { shape :: Shape
                       , cells :: Map Loc Cell
                       } deriving Show

data GameState = GameState { turn :: Int
                           , gameMap :: GameMap
                           , ownPlayer :: Player
                           , allPlayers :: PlayerSet
                           } deriving Show

data GameRec = GameRec { const :: ConstantDict
                       , state :: GameState
                       }

class Id a where
  toInt :: a -> Int

class Located a where
  loc :: a -> Loc

class Halited a where
  halite :: a -> Int


instance Show Direction where
  show = \case
    North -> "n"
    East  -> "e"
    South -> "s"
    West  -> "w"

instance Show Command where
  show = \case
    Spawn          -> "g"
    Construct ship -> "c " ++ (showId ship)
    Collect ship   -> "m " ++ (showId ship) ++ " o"
    Move ship dir  -> "m " ++ (showId ship) ++ " " ++ (show dir)

instance Id PlayerId where
  toInt (PlayerId i) = i

instance Id ShipId where
  toInt (ShipId i) = i

instance Id StructId where
  toInt (StructId i) = i

instance Located Ship where
  loc = shipLoc

instance Located Structure where
  loc = structLoc

instance Located Cell where
  loc = cellLoc

instance Halited Player where
  halite = playerHalite

instance Halited Ship where
  halite = shipHalite

instance Halited Cell where
  halite = cellHalite


cardinals :: [Direction]
cardinals = [North, East, South, West]

showId :: Ship -> String
showId = show . toInt . sid

emptyCell :: Loc -> Int -> Cell
emptyCell loc halite = Cell { cellLoc = loc
                            , cellHalite = halite
                            , ship = Nothing
                            , struct = Nothing
                            }

setHalite :: Int -> Cell -> Cell
setHalite h cell = cell { cellHalite = h }

setDropOff :: Structure -> Cell -> Cell
setDropOff dropOff cell = cell { struct = Just dropOff }

setShip :: Ship -> Cell -> Cell
setShip ship cell = cell { ship = Just ship }

remShip :: Cell -> Cell
remShip cell = cell { ship = Nothing }

isOccupied :: Cell -> Bool
isOccupied = isJust . ship

isShipYard :: Structure -> Bool
isShipYard = (<0) . toInt . structId
