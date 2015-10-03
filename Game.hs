{-# LANGUAGE OverloadedStrings #-}
module Game where

import Bag
import Board
import UI

import qualified Data.Set as Set
import           Data.Set ( Set )


--------------------------------------------------------------------------------
-- Starting locations

data GameSetup = GameSetup
  { allLocations      :: [ Place ]
  , startingLocations :: [ Place ]
  }


{-
gruusWalkabout :: [ Place ]
gruusWalkabout = map mk [ 1, 14, 21 ]
  where mk x = PlaceId { cellNumnber = x, cellSide = A }

floatingInParadise :: [ Place ]
floatingInParadise = map mk [ 2, 16, 18 ]
  where mk x = PlaceId { cellNumnber = x, cellSide = B }

riverOfFreedom :: [ Place ]
riverOfFreedom = map mk [ 5, 15, 19 ]
  where mk x = PlaceId { cellNumnber = x, cellSide = A }
-}
--------------------------------------------------------------------------------





data Game = Game
  { players :: Players
  , board   :: Board Place
  }

preGame :: Circle -> Game
preGame p = Game { players = singlePlayer p, board = emptyBoard }

occupied :: Game -> [(Circle,[Loc])]
occupied g = [ (playerCircle p, ls)
                  | p <- playerList (players g)
                  , let ls = map druidLocation (playerDruids p) ]



data Players = Players
  { playersPrev :: [Player]
  , playerCur   :: CurrentPlayer
  , playersNext :: [Player]
  }

singlePlayer :: Circle -> Players
singlePlayer p = Players
  { playersPrev = []
  , playersNext = []
  , playerCur = CurrentPlayer
      { currentPlayer = newPlayer p
      , currentPhase  = PhaseBegin
      }
  }


playerList :: Players -> [Player]
playerList p = currentPlayer (playerCur p) : playersPrev p ++ playersNext p


data CurrentPlayer = CurrentPlayer
  { currentPlayer :: Player
  , currentPhase  :: Phase
  }

data Phase = PhaseBegin
           | PhaseRefresh Int
           | PhaseAction
           | PhaseEnd




data Player = Player
  { playerCircle        :: Circle
  , playerSeeds       :: Bag Energy
  , playerUnravelled  :: Set Place
  , playerSupply      :: [Druid]
  , playerDruids      :: [ActiveDruid]
  }

newPlayer :: Circle -> Player
newPlayer pn = Player
  { playerCircle     = pn
  , playerSeeds      = bagEmpty
  , playerUnravelled = Set.empty
  , playerSupply     = zipWith druid [ 0 .. ] (Elder : replicate 4 Acolyte)
  , playerDruids     = []
  }
  where
  druid n r = Druid { druidRank = r
                    , druidName = DruidName { druidCircle = pn
                                            , druidNumber = n }
                    }

readyDruids :: Player -> [DruidName]
readyDruids p =
  [ druidName d | ActiveDruid { druidState = Ready
                              , druidId = d } <- playerDruids p ]

exhaustedDruids :: Player -> [DruidName]
exhaustedDruids p =
  [ druidName d | ActiveDruid { druidState = Exhausted
                              , druidId = d } <- playerDruids p ]

inactiveDruids :: Player -> [DruidName]
inactiveDruids p = map druidName (playerSupply p)

remove :: (a -> Bool) -> [a] -> Maybe (a, [a])
remove p xs = case break p xs of
                (as,b:bs) -> Just (b, as ++ bs)
                _         -> Nothing

removeActive :: DruidName -> Player -> Maybe (ActiveDruid, Player)
removeActive nm p =
  do (d,ds) <- remove ((== nm) . druidName . druidId) (playerDruids p)
     return (d, p { playerDruids = ds })

removeInactive :: DruidName -> Player -> Maybe (Druid, Player)
removeInactive nm p =
  do (d,ds) <- remove ((== nm) . druidName) (playerSupply p)
     return (d, p { playerSupply = ds })


exileDruid :: DruidName -> Player -> Maybe Player
exileDruid nm p =
  do (d,p1) <- removeActive nm p
     return p1 { playerSupply = druidId d : playerSupply p1 }

summonDruid :: DruidName -> Loc -> Player -> Maybe Player
summonDruid nm loc p =
  do (d,p1) <- removeInactive nm p
     return p1 { playerDruids = ActiveDruid { druidId       = d
                                            , druidState    = Exhausted
                                            , druidLocation = loc
                                            } : playerDruids p1 }

moveDruid :: DruidName -> Loc -> Player -> Maybe Player
moveDruid nm newLoc p =
  do (d,p1) <- removeActive nm p
     let d1 = d { druidLocation = newLoc }
     return p1 { playerDruids = d1 : playerDruids p1 }


data Druid        = Druid { druidName :: DruidName, druidRank :: DruidRank }

data DruidName      = DruidName { druidCircle :: Circle
                            , druidNumber :: !Int
                            } deriving (Eq,Ord)


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
                   , placeId       :: PlaceId
                   }

instance Eq Place where
  x == y = placeId x == placeId y

instance Ord Place where
  compare x y = compare (placeId x) (placeId y)

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
                  deriving (Eq,Ord)

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




