module Board where

import Data.Maybe(fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


data Dir = N | NE | SE | S | SW | NW
           deriving (Eq,Ord,Show,Enum,Bounded)

allDirs :: [Dir]
allDirs = [ minBound .. maxBound ]

opposite :: Dir -> Dir
opposite = toEnum . (`mod` 6) . (+3) . fromEnum

delta :: Dir -> (Int,Int)
delta d =
  case d of
    N  -> (0,  1)
    NE -> (1,  0)
    SE -> (1, -1)
    S  -> (0, -1)
    SW -> (-1, 0)
    NW -> (-1, 1)

neighbourhood :: Loc -> [Loc]
neighbourhood (x,y) = [ (x + dx, y + dy) | (dx,dy) <- map delta allDirs ]




--------------------------------------------------------------------------------
type Loc      = (Int,Int)
type Board a  = Map Loc a

-- | Get the cell at a location, if any.
getCell :: Loc -> Board a -> Maybe a
getCell = Map.lookup

-- | Set the cell at a location.
setCell :: Loc -> a -> Board a -> Board a
setCell = Map.insert

-- | Remove a cell from the location.
removeCell :: Loc -> Board a -> Board a
removeCell l b = Map.delete l b

-- | Which neighbourhoods are connected.
neighbours :: Board a -> Loc -> [Loc]
neighbours b l = [ l' | l' <- neighbourhood l, l' `Map.member` b ]


-- | The ste of locations reachable from any of the input locations.
reachable :: Board a -> [Loc] -> Set Loc
reachable b = go Set.empty
  where
  go rs [] = rs
  go rs (x : xs)
    | x `Set.member` rs = go rs xs
    | otherwise         = go (Set.insert x rs) (neighbours b x ++ xs)

-- | A cell is locked if removing it would split the graph into two components.
locked :: Board a -> Loc -> Bool
locked b l =
  case neighbours b l of
    []    -> True
    n : _ -> reachable b' [n] /= Map.keysSet b'
      where b' = removeCell l b

-- | Swap two cells on the board.
swapCells :: Loc -> Loc -> Board a -> Board a
swapCells l1 l2 b =
  fromMaybe b $
  do c1 <- getCell l1 b
     c2 <- getCell l2 b
     return (setCell l1 c2 (setCell l2 c1 b))



