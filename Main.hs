{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Game
import           UI
import           Board
import           JSON

import           Snap.Http.Server (quickHttpServe)
import           Snap.Core (Snap)
import qualified Snap.Core as Snap
import           Snap.Util.FileServe(serveDirectory)

import           Data.IORef ( IORef, newIORef, writeIORef, readIORef )

import           Control.Applicative ((<|>))
import           Control.Monad.IO.Class(liftIO)


main :: IO ()
main =
  do s <- newIORef newGame

     quickHttpServe $ Snap.route
       [ ("step", snapStep s)
       , ("reset", liftIO (writeIORef s newGame))
       ] <|> serveDirectory "ui"


snapStep :: IORef (I ()) -> Snap ()
snapStep s =
  do m  <- liftIO (readIORef s)
     m1 <- snapStepI m
     liftIO (writeIORef s m1)

newGame :: I ()
newGame = runGameM [Stag] g
  where
  g = addTile (Loc 0 0) (places !! 0)


