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
  do let it = do x <- cmdChooseTile [ Loc 0 1, Loc 1 0 ]
                 y <- cmdChooseNewLocation [ "2A", "2B" ]
                 cmdAppear x y
                 it
     s <- newIORef it

     quickHttpServe $ Snap.route
       [ ("step", snapStep s)
       , ("reset", liftIO (writeIORef s it))
       ] <|> serveDirectory "ui"


snapStep :: IORef (I ()) -> Snap ()
snapStep s =
  do w  <- liftIO (readIORef s)
     w1 <- snapStepI w
     liftIO (writeIORef s w1)



