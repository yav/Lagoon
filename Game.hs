{-# LANGUAGE OverloadedStrings #-}
module Game where

import Bag
import Board
import UI

import           Data.Monoid((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import           Data.Set ( Set )



data Game = Game
  { players :: Players
  , board   :: Board PlaceId
  }

occupied :: Game -> [(Text,[Loc])]
occupied g = [ (playerName p, ls)
                  | p <- playerList (players g)
                  , let ls = map druidLocation (playerDruids p) ]



data Players = Players
  { playersPrev :: [Player]
  , playerCur   :: CurrentPlayer
  , playersNext :: [Player]
  }

playerList :: Players -> [Player]
playerList p = currentPlayer (playerCur p) : playersPrev p ++ playersNext p


data CurrentPlayer = CurrentPlayer
  { currentPlayer :: Player
  , currentPhase  :: Phase
  }

data Phase = PhaseBegin
           | PhaseRefresh
           | PhaseAction
           | PhaseEnd


data Player = Player
  { playerName        :: Text
  , playerSeeds       :: Bag Energy
  , playerUnravelled  :: Set PlaceId
  , playerSupply      :: [Druid]
  , playerDruids      :: [ActiveDruid]
  }

newPlayer :: Text -> Player
newPlayer pn = Player
  { playerName       = pn
  , playerSeeds      = bagEmpty
  , playerUnravelled = Set.empty
  , playerSupply     = zipWith druid [ 0 .. ] (Elder : replicate 4 Acolyte)
  , playerDruids     = []
  }
  where
  druid n r = Druid { druidRank = r
                    , druidName = pn <> "_" <> Text.pack (show (n :: Int))
                    }

readyDruids :: Player -> [Text]
readyDruids p =
  [ druidName d | ActiveDruid { druidState = Ready
                              , druidId = d } <- playerDruids p ]

exhaustedDruids :: Player -> [Text]
exhaustedDruids p =
  [ druidName d | ActiveDruid { druidState = Exhausted
                              , druidId = d } <- playerDruids p ]

inactiveDruids :: Player -> [Text]
inactiveDruids p = map druidName (playerSupply p)


removeActive :: Text -> Player -> Maybe (ActiveDruid, Player)
removeActive nm p =
  case break ((== nm) . druidName . druidId) (playerDruids p) of
    (as, b : bs) -> Just (b, p { playerDruids = as ++ bs })
    _            -> Nothing

removeInactive :: Text -> Player -> Maybe (Druid, Player)
removeInactive nm p =
  case break ((== nm) . druidName) (playerSupply p) of
    (as, b : bs) -> Just (b, p { playerSupply = as ++ bs })
    _            -> Nothing


exileDruid :: Text -> Player -> Maybe Player
exileDruid nm p =
  do (d,p1) <- removeActive nm p
     return p1 { playerSupply = druidId d : playerSupply p1 }

summonDruid :: Text -> Loc -> Player -> Maybe Player
summonDruid nm loc p =
  do (d,p1) <- removeInactive nm p
     return p1 { playerDruids = ActiveDruid { druidId       = d
                                            , druidState    = Exhausted
                                            , druidLocation = loc
                                            } : playerDruids p1 }

moveDruid :: Text -> Loc -> Player -> Maybe Player
moveDruid nm newLoc p =
  do (d,p1) <- removeActive nm p
     let d1 = d { druidLocation = newLoc }
     return p1 { playerDruids = d1 : playerDruids p1 }


data Druid        = Druid { druidName :: Text, druidRank :: DruidRank }
                    deriving (Eq,Ord)

data DruidRank    = Acolyte | Elder
                    deriving (Eq,Ord)

data DruidState   = Ready | Exhausted

data ActiveDruid  = ActiveDruid
  { druidId       :: Druid
  , druidState    :: DruidState
  , druidLocation :: Loc
  }


data Place = Place { placeEnergy   :: Energy
                   , placeIsHaven  :: Bool
                   , placeGroup    :: Int
                   , placeName     :: String
                   }

data PlaceId    = PlaceId { cellNumnber :: Int, cellSide :: Side }
                  deriving (Eq,Ord)

data Side       = A | B
                  deriving (Eq,Ord)

data Energy     = Yellow | Red | Blue
                  deriving (Eq,Ord)

data LocType    = Allay | Haven | Sanctum | Shrine | Trove

data ActionType = Invokation
                | EldridInvokation
                | Event Event
                | Exploration

data Event      = WhenAnyoneUnravels
                | WhenIUravel
                | WhenIBegin
                | OnMyTurnOnce
                | WhenIEnd
                | WhenSuplyEmpty
                | OnMyActionOnce


data Path       = Adventure | Renewal | Presence | Mystery
data Circle     = Stag | Dragonfly
                | Hare | Owl
                | Fern | Turtle
                | Mushroom | Moon

pathOf :: Circle -> Path
pathOf c =
  case c of
    Stag      -> Adventure
    Dragonfly -> Adventure
    Hare      -> Renewal
    Owl       -> Renewal
    Fern      -> Presence
    Turtle    -> Presence
    Mushroom  -> Mystery
    Moon      -> Mystery


--------------------------------------------------------------------------------
-- AJ




