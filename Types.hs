module Types where

import Bag
import Board

import Data.Text (Text)
import Data.Set ( Set )
import Data.Map ( Map )


data Game = Game
  { players :: Map Circle Player
  , board   :: Board ActivePlace
  -- XXX: Player order, current player
  }

data Player = Player
  { playerCircle      :: Circle
  , playerSeeds       :: Bag Energy
  , playerUnravelled  :: Set Place
  , playerSupply      :: Set Druid
  }



--------------------------------------------------------------------------------
-- Tiles

-- | A place in the bag
data Place        = Place { letPlaceEnergy  :: Energy
                          , letPlaceIsHaven :: Bool
                          , letPlaceGroup   :: Int
                          , letPlaceName    :: Text
                          , letPlaceId      :: PlaceId
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


data ActivePlace  = ActivePlace { placeStatic :: Place
                                , placeDruids :: [ActiveDruid]
                                }

class    IsPlace t           where place :: t -> Place
instance IsPlace Place       where place = id
instance IsPlace ActivePlace where place = placeStatic

placeEnergy  :: IsPlace t => t -> Energy
placeEnergy = letPlaceEnergy . place

placeIsHaven :: IsPlace t => t -> Bool
placeIsHaven = letPlaceIsHaven . place

placeGroup :: IsPlace t => t -> Int
placeGroup = letPlaceGroup . place

placeName :: IsPlace t => t -> Text
placeName = letPlaceName . place

placeId :: IsPlace t => t -> PlaceId
placeId = letPlaceId . place




--------------------------------------------------------------------------------
-- Tokens


-- | A token in a player's supply
data Druid        = Druid { letDruidName :: DruidName
                          , letDruidRank :: DruidRank
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
                                }

data DruidState   = Ready | Exhausted
                    deriving Eq

class    IsDruid t           where druid :: t -> Druid
instance IsDruid ActiveDruid where druid = druidStatic
instance IsDruid Druid       where druid = id

druidName :: IsDruid t => t -> DruidName
druidName = letDruidName . druid

druidRank :: IsDruid t => t -> DruidRank
druidRank = letDruidRank . druid





