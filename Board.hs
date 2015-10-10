{-# LANGUAGE OverloadedStrings #-}
module Board where

import JSON

import Data.Maybe(fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List(nub)
import Data.Foldable(traverse_)


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
neighbourhood (Loc x y) =
  [ Loc (x + dx) (y + dy) | (dx,dy) <- map delta allDirs ]




--------------------------------------------------------------------------------
data Loc  = Loc Int Int
            deriving (Eq,Ord)

instance Export Loc where
  toJS (Loc x y) = object [ "x" .= x, "y" .= y ]

instance Import Loc where
  fromJS = withObject $ \o -> do x <- o .: "x"
                                 y <- o .: "y"
                                 return (Loc x y)


newtype Board a = Board (Map Loc a)


emptyBoard :: Board a
emptyBoard = Board Map.empty

-- | Get the cell at a location, if any.
getCell :: Loc -> Board a -> Maybe a
getCell l (Board b) = Map.lookup l b

-- | Set the cell at a location.
setCell :: Loc -> a -> Board a -> Board a
setCell l a (Board b) = Board (Map.insert l a b)

-- | Remove a cell from the location.
removeCell :: Loc -> Board a -> Board a
removeCell l (Board b) = Board (Map.delete l b)

-- | Which neighbourhoods are connected.
neighbours :: Board a -> Loc -> [Loc]
neighbours (Board b) l = [ l' | l' <- neighbourhood l, l' `Map.member` b ]

neighboursN :: Board a -> Int -> Loc -> [Loc]
neighboursN b n l
  | n > 1     = let ls = neighbours b l
                in nub (ls ++ [ x | l' <- ls, x <- neighboursN b (n-1) l'
                                  , x /= l ])
  | otherwise = neighbours b l



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
    n : _ -> reachable b' [n] /= boardFilled b'
      where b' = removeCell l b

boardFilled :: Board a -> Set Loc
boardFilled (Board mp) = Map.keysSet mp

-- | Swap two cells on the board.
swapCells :: Loc -> Loc -> Board a -> Board a
swapCells l1 l2 b =
  fromMaybe b $
  do c1 <- getCell l1 b
     c2 <- getCell l2 b
     return (setCell l1 c2 (setCell l2 c1 b))

forEach_ :: Applicative m => Board a -> (Loc -> a -> m ()) -> m ()
forEach_ (Board m) f = traverse_ (uncurry f) (Map.toList m)


