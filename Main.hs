{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           JSON
import           Game

import           Snap.Http.Server (quickHttpServe)
import           Snap.Core (Snap)
import qualified Snap.Core as Snap
import           Snap.Util.FileServe(serveDirectory)


import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
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


data Loc  = Loc Int Int

instance Export Loc where
  toJS (Loc x y) = object [ "x" .= x, "y" .= y ]

instance Import Loc where
  fromJS = withObject $ \o -> do x <- o .: "x"
                                 y <- o .: "y"
                                 return (Loc x y)


cmdAppear :: Loc -> Text -> I ()
cmdAppear = remote "appear"

cmdDisappear :: Loc -> I ()
cmdDisappear = remote "disappear"

cmdSwapTiles :: Loc -> Loc -> I ()
cmdSwapTiles = remote "swapTiles"

cmdMoveTile :: Loc -> Loc -> I ()
cmdMoveTile = remote "moveTile"

cmdAddDruid :: Text -> Loc -> Text -> Bool -> Bool -> I ()
cmdAddDruid = remote "addDruid"

cmdRemoveDruid :: Text -> I ()
cmdRemoveDruid = remote "removeDruid"

cmdMoveDruid :: Text -> Loc -> I ()
cmdMoveDruid = remote "moveDruid"

cmdSetDruidState :: Text -> Bool -> I ()
cmdSetDruidState = remote "setDruidState"

cmdChooseTile :: [Loc] -> I Loc
cmdChooseTile = remote "chooseTile"

cmdChooseDruid :: [Text] -> I Text
cmdChooseDruid = remote "chooseDruid"

cmdChooseNewLocation :: [Text] -> I Text
cmdChooseNewLocation = remote "chooseNewLocation"



--------------------------------------------------------------------------------

requiredParam :: ByteString -> Snap ByteString
requiredParam p =
  do mb <- Snap.getParam p
     case mb of
       Just x  -> return x
       Nothing -> badInput ("Missing parameter: " `BS.append` p)

sendJSON :: Export a => a -> Snap ()
sendJSON a =
  do Snap.modifyResponse (Snap.setHeader "content-type" "application/json")
     Snap.writeLBS (jsonBytes a)


