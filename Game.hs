{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Game where

import Types
import Board
import Bag

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad(guard)


--------------------------------------------------------------------------------
-- Game


-- | Game with a blank map and no players.
blankGame :: Game
blankGame = Game { players = Map.empty, board = emptyBoard }

-- | Add a player to a game.
addPlayer :: Circle -> Player -> Game -> Game
addPlayer c p g = g { players = Map.insert c p (players g) }

-- | Find an active druid.
findDruid :: DruidName -> Game -> Maybe (Loc,ActivePlace)
findDruid nm g = findLoc (board g) thisOne
  where thisOne = any ((nm == ) . druidName) . placeDruids

-- | Add a druid at the given location.  Fails if there is no such location.
addDruidAt :: ActiveDruid -> Loc -> Game -> Maybe Game
addDruidAt d l g =
  do let b = board g
     p <- getCell l b
     return g { board = setCell l (addDruid d p) b }

-- | Remove a druid, returng it, and where it used to be.
removeDruid :: DruidName -> Game -> Maybe (Loc, ActiveDruid, Game)
removeDruid nm g =
    do (loc,p) <- findDruid nm g
       (d,p1)  <- removeDruid' nm p
       return (loc, d, g { board = setCell loc p1 (board g) })

-- | Move a druid from one location to another.  Fails if the druid
-- did not exits, or the new location does not exist.
moveDruid :: DruidName -> Loc -> Game -> Maybe Game
moveDruid nm to g =
  do (_,d,g1) <- removeDruid nm g
     addDruidAt d to g1

-- | Set the state of an active druid.  Fails if there is no such druid.
setDruidState :: DruidState -> DruidName -> Game -> Maybe Game
setDruidState s nm g =
  do (l,d,g1) <- removeDruid nm g
     let b = board g1
     p <- getCell l b
     let d1 = d { druidState = s }
     return g1 { board = setCell l (addDruid d1 p) b }

-- | Summon an exhausted druid for the given player at the given location.
-- The location must exist, and should be a haven.
summon :: Circle -> DruidName -> Loc -> Game -> Maybe Game
summon c nm loc g =
  do p      <- Map.lookup c (players g)
     (d,p1) <- prepareToSummon nm p
     ap     <- getCell loc (board g)
     guard (placeIsHaven ap)
     let d1  = ActiveDruid { druidState = Exhausted, druidStatic = d }
         ap1 = addDruid d1 ap
     return g { players = Map.insert c p1 (players g)
              , board   = setCell loc ap1 (board g)
              }


--------------------------------------------------------------------------------
-- Player

-- | A new player, with no points and all druids in the supply.
newPlayer :: Circle -> Player
newPlayer c = Player
  { playerCircle     = c
  , playerSeeds      = bagEmpty
  , playerUnravelled = Set.empty
  , playerSupply     = Set.fromList
                         (zipWith new [ 0 .. ] (Elder : replicate 4 Acolyte))
  }
  where
  new n r = Druid { letDruidRank = r
                  , letDruidName = DruidName { druidCircle = c
                                             , druidNumber = n }
                  }


-- | Modify the seeds by the given amount.  Fails if we would decrease the
-- seeds to a negative quantity.
updateSeeds :: Int -> Energy -> Player -> Maybe Player
updateSeeds n e p =
  case compare n 0 of
    LT -> do s <- bagRemove (negate n) e (playerSeeds p)
             return p { playerSeeds = s }
    EQ -> Just p
    GT -> Just p { playerSeeds = bagAdd n e (playerSeeds p) }

prepareToSummon :: DruidName -> Player -> Maybe (Druid, Player)
prepareToSummon nm p =
  case Set.partition ((nm ==) . druidName) (playerSupply p) of
    (as,bs) -> do (a,_) <- Set.minView as
                  return (a, p { playerSupply = bs })

--------------------------------------------------------------------------------
-- Active Place

addDruid :: ActiveDruid -> ActivePlace -> ActivePlace
addDruid d a = a { placeDruids = d : placeDruids a }

removeDruid' :: DruidName -> ActivePlace -> Maybe (ActiveDruid, ActivePlace)
removeDruid' nm a =
    do (d,xs) <- rm (placeDruids a)
       return (d, a { placeDruids = xs })
  where
  rm xs = case xs of
            []     -> Nothing
            b : bs -> if druidName b == nm
                        then Just (b, bs)
                        else do (c,cs) <- rm bs
                                return (c, b : cs)






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


{-

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

-}


