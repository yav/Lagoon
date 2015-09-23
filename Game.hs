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


exile :: Int -> Player -> Player
exile n p =
  case splitAt n (playerDruids p) of
    (as,b:bs) -> p { playerSupply = bagAdd 1 (druidType b) (playerSupply p)
                   , playerDruids = as ++ bs
                   }
    _ -> p

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

data PlaceId     = PlaceId { cellNumnber :: Int, cellSide :: Side }
                  deriving (Eq,Ord)

data Side       = A | B
                  deriving (Eq,Ord)

data Energy     = Yellow | Red | Blue
                  deriving (Eq,Ord)

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

--------------------------------------------------------------------------------

{-
places :: Map PlaceId Place
places = Map.fromList
  [

  , ( PlaceId 5 B
    , Place { placeEnergy   = Blue
            , placeIsHaven  = False
            , placeGroup    = 2
            , placeName     = "Musing Kinoko"
            }
    )


  ]



move = do druid <- chooseReadyDruid
          dir   <- chooseMoveDir (only dir with placlas)
          tryToMove druid dir
          -- exhaust druid

summon = do d1 <- pickReadyDruid
            d <- pickDruidFromSupply 
                  (only need to know Eldrid/Acolyte if this is an option)
            h <- pickHaven (if there is more than one)


explore = do druid <- chooseReadyDruid
             dr <- chooseDir (only dir with no places)
             bool <- shouldWeMoveToNewSite
             exhause druid and player marker

siteAction =
  do chooseOneOfOurCouupiedSites


-}

