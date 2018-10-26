{-# LANGUAGE LambdaCase #-}

module Hlt.Constants where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Text (pack)

type ConstantDict = Object

data ConstantKey = DropOffCost
                 | ExtractRatio
                 | GameSeed
                 | InspirationEnabled
                 | InspirationRadius
                 | InspirationShipCount
                 | InspiredBonusMultiplier
                 | InspiredExtractRatio
                 | InspiredMoveCostRatio
                 | MaxHalite
                 | MaxTurns
                 | MoveCostRatio
                 | ShipCost
                 | ConstKey String

instance Show ConstantKey where
  show = \case
    DropOffCost             -> "DROPOFF_COST"
    ExtractRatio            -> "EXTRACT_RATIO"
    GameSeed                -> "game_seed"
    InspirationEnabled      -> "INSPIRATION_ENABLED"
    InspirationRadius       -> "INSPIRATION_RADIUS"
    InspirationShipCount    -> "INSPIRATION_SHIP_COUNT"
    InspiredBonusMultiplier -> "INSPIRED_BONUS_MULTIPLIER"
    InspiredExtractRatio    -> "INSPIRED_EXTRACT_COST"
    InspiredMoveCostRatio   -> "INSPIRED_MOVE_COST_RATIO"
    MaxHalite               -> "MAX_ENERGY"
    MaxTurns                -> "MAX_TURNS"
    MoveCostRatio           -> "MOVE_COST_RATIO"
    ShipCost                -> "NEW_ENTITY_ENERGY_COST"
    ConstKey s              -> s


parser :: FromJSON a => ConstantKey -> Object -> Maybe a
parser key = parseMaybe $ flip (.:) (pack $ show key)

parseConstants :: IO ConstantDict
parseConstants = fromJust . decode . LB.fromStrict <$> B.getLine

getMaybeBool :: ConstantKey -> ConstantDict -> Maybe Bool
getMaybeBool = parser

getBool :: ConstantKey -> ConstantDict -> Bool
getBool key = fromJust. getMaybeBool key

getMaybeInt :: ConstantKey -> ConstantDict -> Maybe Int
getMaybeInt = parser

getInt :: ConstantKey -> ConstantDict -> Int
getInt key = fromJust . getMaybeInt key

getMaybeDouble :: ConstantKey -> ConstantDict -> Maybe Double
getMaybeDouble = parser

getDouble :: ConstantKey -> ConstantDict -> Double
getDouble key = fromJust . getMaybeDouble key
