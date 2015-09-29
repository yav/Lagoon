module Game where

import Bag
import Board
import Data.Set (Set)
import qualified Data.Map as Map
import           Data.Map ( Map )


data Game = Game
  { players :: Players
  , board   :: Board PlaceId
  }

occupied :: Game -> [(PlayerName,[Loc])]
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

newtype PlayerName = PlayerName String

data Player = Player
  { playerName        :: PlayerName
  , playerSeeds       :: Bag Energy
  , playerUnravelled  :: Set PlaceId
  , playerSupply      :: Bag Druid
  , playerDruids      :: [ActiveDruid]
  }

readyDruids :: Player -> [Int]
readyDruids p =
  [ n | (n, ActiveDruid { druidState = Ready }) <-
                                              zip [ 0 .. ] (playerDruids p) ]

exhaustedDruids :: Player -> [Int]
exhaustedDruids p =
  [ n | (n, ActiveDruid { druidState = Exhausted }) <-
                                              zip [ 0 .. ] (playerDruids p) ]


exileDruid :: Int -> Player -> Maybe Player
exileDruid n p =
  do (as,b:bs) <- return $ splitAt n (playerDruids p)
     return p { playerSupply = bagAdd 1 (druidType b) (playerSupply p)
              , playerDruids = as ++ bs
              }

summonDruid :: Druid -> Loc -> Player -> Maybe Player
summonDruid druid loc p =
  do newSupply <- bagRemove 1 druid (playerSupply p)
     return p { playerSupply = newSupply
              , playerDruids = ActiveDruid { druidType     = druid
                                           , druidState    = Exhausted
                                           , druidLocation = loc
                                           } : playerDruids p }

moveDruid :: Int -> Loc -> Player -> Maybe Player
moveDruid n newLoc p =
  do (as,b:bs) <- return (splitAt n (playerDruids p))
     return p { playerDruids = as ++ b { druidLocation = newLoc } : bs }


data Druid        = Acolyte | Elder
                    deriving (Eq,Ord)

data DruidState   = Ready | Exhausted

data ActiveDruid  = ActiveDruid
  { druidType     :: Druid
  , druidState    :: DruidState
  , druidLocation :: Loc
  }

activeMove :: Loc -> ActiveDruid -> ActiveDruid
activeMove l a = a { druidLocation = l }

activeExhaust :: ActiveDruid -> ActiveDruid
activeExhaust a = a { druidState = Exhausted }

activeRefresh :: ActiveDruid -> ActiveDruid
activeRefresh a = a { druidState = Ready }



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




