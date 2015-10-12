{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Game where

import Bag
import Board
import UI
import Perhaps
import JSON

import           Data.Foldable(for_)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import           Data.Set ( Set )
import           Data.List(nub)
import           Control.Monad(liftM,ap)


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

newtype GameM a = GameM (Game -> I (a, Game))

instance Functor GameM where
  fmap = liftM

instance Applicative GameM where
  pure a = GameM (\g -> return (a, g))
  (<*>)  = ap

instance Monad GameM where
  GameM m >>= k = GameM (\g -> do (a,g1) <- m g
                                  let GameM m1 = k a
                                  m1 g1)

runGameM :: [Circle] -> GameM a -> I a
runGameM ps (GameM m) =
  case blankGame ps of
    Failed err -> fail (Text.unpack err)
    Ok g       -> fmap fst (m g)

ict :: I a -> GameM a
ict m = GameM (\g -> do x <- m
                        return (x,g))

withGame :: (Game -> (a,Game)) -> GameM a
withGame f = GameM (return . f)

withGame_ :: (Game -> Game) -> GameM ()
withGame_ f = GameM (\g -> return ((), f g))



addTile :: Loc -> Place -> GameM ()
addTile l p =
  do ict $ cmdAppear l (placeIdText p)
     withGame_ $ \Game { .. } -> Game { board = setCell l p board, .. }

{-
addDruid :: Loc -> DruidName -> GameM ()
addDruid l d =
  do withGame $ \Game { .. } ->
      let CurrentPlayer { .. } = playerCur
      in case summonDruid d l currentPlayer of
-}


data Game = Game
  { players :: Players
  , board   :: Board Place
  }


{-
addDruid :: :: Game -> I ()
sendInitial Game { .. } =
  do cmdReset
     forEach_ board $ \l p -> cmdAppear l (placeIdText p)
     for_ (playerList players) $ \p ->
      for_ (playerDruids p) $ \d ->
        do let ActiveDruid { druidId = Druid { druidName = DruidName { .. }
                                             , ..}
                           , .. } = d
           cmdAddDruid
             (druidTextId d)
             druidLocation
             (Text.pack (show druidCircle))
             (druidRank == Acolyte)
             (druidState == Ready)


preGame :: Circle -> Game
preGame p = Game { players = singlePlayer p, board = emptyBoard }
-}
occupied :: Game -> [(Circle,[Loc])]
occupied g = [ (playerCircle p, ls)
                  | p <- playerList (players g)
                  , let ls = map druidLocation (playerDruids p) ]



blankGame :: [Circle] -> Perhaps Game
blankGame [] = Failed "Cannot start a game with no players"
blankGame ps | nub ps /= ps = Failed "Duplicate players"
blankGame (p : ps) = Ok (foldr add g0 ps)
  where
  g0 = Game { players = singlePlayer p, board = emptyBoard }
  add c Game { .. } = Game { players = addPlayer c players, .. }



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

addPlayer :: Circle -> Players -> Players
addPlayer c Players { .. } = Players { playersNext = newPlayer c : playersNext
                                     , .. }

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

instance Export DruidName where
  toJS DruidName { .. } =
    toJS (Text.pack (show druidCircle ++ show druidNumber))


data DruidRank    = Acolyte | Elder
                    deriving (Eq,Ord)

data DruidState   = Ready | Exhausted
                    deriving Eq

data ActiveDruid  = ActiveDruid
  { druidId       :: Druid
  , druidState    :: DruidState
  , druidLocation :: Loc
  }

data Place = Place { placeEnergy   :: Energy
                   , placeIsHaven  :: Bool
                   , placeGroup    :: Int
                   , placeName     :: Text
                   , placeId       :: PlaceId
                   }

instance Eq Place where
  x == y = placeId x == placeId y

instance Ord Place where
  compare x y = compare (placeId x) (placeId y)

data PlaceId    = PlaceId { cellNumnber :: Int, cellSide :: Side }
                  deriving (Eq,Ord)

placeIdText :: Place -> Text
placeIdText Place { placeId = PlaceId { .. } } =
  Text.pack (show cellNumnber ++ side)
  where side = case cellSide of
                 A -> "A"
                 B -> "B"


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
                  deriving (Eq,Ord,Show)

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

places :: [ Place ]
places =
  [ 
    -- 1
    Place { placeEnergy   = Yellow
          , placeIsHaven  = True
          , placeGroup    = 1
          , placeName     = "Gruu's Refuge"
          , placeId       = pid 1 A
          }

  , Place { placeEnergy   = Red
          , placeIsHaven  = True
          , placeGroup    = 1
          , placeName     = "Cloudtop Monastery"
          , placeId       = pid 1 B
          }


    -- 14
  , Place { placeEnergy   = Red
          , placeIsHaven  = False
          , placeGroup    = 2
          , placeName     = "Eye of the Forest"
          , placeId       = pid 14 A
          }

  , Place { placeEnergy   = Blue
          , placeIsHaven  = False
          , placeGroup    = 2
          , placeName     = "Moon Garden"
          , placeId       = pid 14 B
          }


    -- 21
  , Place { placeEnergy   = Blue
          , placeIsHaven  = False
          , placeGroup    = 3
          , placeName     = "Terrapin Ancient"
          , placeId       = pid 21 A
          }

  , Place { placeEnergy   = Yellow
          , placeIsHaven  = False
          , placeGroup    = 3
          , placeName     = "Kindred Stone"
          , placeId       = pid 21 B
          }
  ]

  where
  pid cellNumnber cellSide = PlaceId { .. }



--------------------------------------------------------------------------------
-- AJ




