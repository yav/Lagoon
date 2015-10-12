module Types where

import Bag
import Board

import Data.Text (Text)
import Data.Set ( Set )
import Data.Map ( Map )


data Game = Game
  { players :: Map Circle Player
  , board   :: Board Place
  }

data Player = Player
  { playerSeeds       :: Bag Energy
  , playerUnravelled  :: Set Place
  , playerSupply      :: Set Druid
  }



--------------------------------------------------------------------------------
-- Tiles

-- | A place in the bag
data Place        = Place { placeEnergy   :: Energy
                          , placeIsHaven  :: Bool
                          , placeGroup    :: Int
                          , placeName     :: Text
                          , placeId       :: PlaceId
                          }

instance Eq Place where
  x == y = placeId x == placeId y

instance Ord Place where
  compare x y = compare (placeId x) (placeId y)

data PlaceId      = PlaceId { cellNumnber :: Int
                            , cellSide    :: Side
                            } deriving (Eq,Ord)

data Side         = A | B
                    deriving (Eq,Ord)

data Energy       = Yellow | Red | Blue
                    deriving (Eq,Ord)

data LocType      = Allay | Haven | Sanctum | Shrine | Trove


data ActivePlace  = ActivePlace { placeStatic :: PlaceId
                                , placeDruids :: [ActiveDruid]
                                }


--------------------------------------------------------------------------------
-- Tokens


-- | A token in a player's supply
data Druid        = Druid { druidName :: DruidName
                          , druidRank :: DruidRank
                          }

instance Eq Druid where
  x == y = druidName x == druidName y

instance Ord Druid where
  compare x y = compare (druidName x) (druidName y)



data DruidName    = DruidName { druidCircle :: Circle
                              , druidNumber :: !Int
                              } deriving (Eq,Ord)

data DruidRank    = Acolyte | Elder
                    deriving (Eq,Ord)



data Path         = Adventure | Renewal | Presence | Mystery

data Circle       = Stag | Dragonfly
                  | Hare | Owl
                  | Fern | Turtle
                  | Mushroom | Moon
                    deriving (Eq,Ord,Show)

-- | A token on the board
data ActiveDruid  = ActiveDruid { druidStatic   :: Druid
                                , druidState    :: DruidState
                                , druidLocation :: Loc
                                }

data DruidState   = Ready | Exhausted
                    deriving Eq






