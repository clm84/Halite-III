module Hlt.Game where

import Control.Monad.Reader

import Hlt.Utils
import Hlt.Constants
import Hlt.Types
import Hlt.Parser

runGame :: String -> GameEnv [Command] -> IO ()
runGame botName turnLogic = do
  const <- parseConstants
  init <- parseInit
  flush botName
  loop turnLogic const init

loop :: GameEnv [Command] -> ConstantDict -> GameState -> IO ()
loop turnLogic const state = do
  newState <- parseTurn (getInt MaxHalite const) state
  sendCommands $ runReader turnLogic (GameRec const newState)
  loop turnLogic const newState

sendCommands :: [Command] -> IO ()
sendCommands = flush . unwords . map show


