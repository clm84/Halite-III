module Hlt.Parser
    ( parseInit
    , parseTurn
    ) where


import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack)
import qualified Data.List as L
import Data.Either (fromRight)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Merge.Strict as Me
import qualified Data.Map.Strict as M
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec.ByteString as SA

import Hlt.Utils
import Hlt.Types


parseInit :: IO GameState
parseInit = SA.parseFromStream pGameState S.stdin

parseTurn :: Int -> GameState -> IO GameState
parseTurn maxHalite prevState = SA.parseFromStream (pTurnGameState maxHalite prevState) S.stdin


pId :: (Int -> a) -> Parser a
pId f = f <$> decimal

pLoc :: Parser Loc
pLoc = do
  x <- decimal <* space
  y <- decimal
  return (x, y)

pPlayer :: Parser Player
pPlayer = do
  pid <- pId PlayerId <* space
  loc <- pLoc <* endOfLine
  let structId = StructId . ((-1)*) . toInt $ pid
  return Player { pid = pid
                , playerHalite = 0
                , shipYard = Structure structId  pid loc
                , dropOffs = M.empty
                , ships = M.empty
                }

pPlayers :: Int -> Parser PlayerSet
pPlayers n = toMap pid <$> count n pPlayer

pGameMap :: Shape -> Parser GameMap
pGameMap (w, h) = do
  rows <- count w $ count h (decimal <* space) <* endOfLine
  return $ GameMap (w, h) (M.fromList . map aux . L.zip indices . L.concat $ rows)
  where
    aux (l, h) = (l, emptyCell l h)
    indices = cart [0..(w-1)] [0..(h-1)]

pGameState :: Parser GameState
pGameState = do
  numPlayers <- decimal <* space
  ownId <- pId PlayerId <* endOfLine
  players <- pPlayers numPlayers
  width <- decimal <* space
  height <- decimal <* endOfLine
  gameMap <- pGameMap (width, height)
  return GameState { turn = 0
                   , gameMap = gameMap
                   , ownPlayer = players ! ownId
                   , allPlayers = players
                   }

pShip :: Int -> PlayerId -> Parser Ship
pShip maxHalite pid = do
  sid <- pId ShipId <* space
  loc <- pLoc <* space
  halite <- decimal <* endOfLine
  return Ship { sid = sid
              , shipPid = pid
              , shipLoc = loc
              , shipHalite = halite
              , isFull = halite >= maxHalite
              }

pDropOff :: PlayerId -> Parser Structure
pDropOff pid = do
  structId <- pId StructId <* space
  loc <- pLoc <* endOfLine
  return Structure { structId = structId
                   , structPid = pid
                   , structLoc = loc
                   }

pTurnPlayer :: Int -> PlayerSet -> Parser Player
pTurnPlayer mh players = do
  pid <- pId PlayerId <* space
  numShips <- decimal <* space
  numDropOffs <- decimal <* space
  halite <- decimal <* endOfLine
  ships <- toMap sid <$> count numShips (pShip mh pid)
  dropOffs <- toMap structId <$> count numDropOffs (pDropOff pid)
  return Player { pid = pid
                , playerHalite = halite
                , shipYard = shipYard $ players ! pid
                , dropOffs = dropOffs
                , ships = ships
                }

pTurnPlayers :: Int -> PlayerSet -> Parser PlayerSet
pTurnPlayers mh prevSet = toMap pid <$> count (M.size prevSet) (pTurnPlayer mh prevSet)

pCellUpdate :: Parser Cell
pCellUpdate = do
  loc <- pLoc <* space
  halite <- decimal <* endOfLine
  return $ emptyCell loc halite

pTurnGameMap :: GameMap -> Parser (Map Loc Cell)
pTurnGameMap prevMap = do
  numUpdates <- decimal <* endOfLine
  updates <- toMap cellLoc <$> count numUpdates pCellUpdate
  return $ M.unionWith aux (cells prevMap) updates
  where aux a b = setHalite (cellHalite b) a

updateMap :: PlayerSet -> Map Loc Cell -> Map Loc Cell
updateMap players prevMap = Me.merge dropShip Me.dropMissing updateShip postDO allShips
  where
    updateShip = Me.zipWithMatched (\l c s -> setShip s c)
    dropShip = Me.mapMissing (\l c -> remShip c)
    postDO = L.foldl (\a e -> M.adjust (setDropOff e) (structLoc e) a) prevMap allDropOffs
    allShips = toMap shipLoc . L.concat . L.map (M.elems . ships) $ playerList
    allDropOffs = L.concat . L.map (M.elems . dropOffs) $ playerList
    playerList = M.elems players

pTurnGameState :: Int -> GameState -> Parser GameState
pTurnGameState mh prevState = do
  turn <- decimal <* endOfLine
  players <- pTurnPlayers mh $ allPlayers prevState
  cells <- updateMap players <$> (pTurnGameMap $ gameMap prevState)
  return GameState { turn = turn
                   , gameMap = GameMap (shape . gameMap $ prevState) cells
                   , ownPlayer = players ! (pid . ownPlayer $ prevState)
                   , allPlayers = players
                   }
