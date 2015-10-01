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
import           Data.Text.Encoding(decodeUtf8)
import           Data.Text.Read(decimal)
import qualified Data.Text as Text
import           Data.IORef ( IORef, newIORef, writeIORef, atomicModifyIORef'
                            , readIORef )

import           Control.Applicative ((<|>))
import           Control.Monad
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

data I a  = GetText (Text -> I a)
          | GetLoc (Loc -> I a)
          | Call Cmd (I a)
          | Return a


instance Functor I where
  fmap = liftM

instance Applicative I where
  pure = return
  (<*>) = ap

instance Monad I where
  return = Return
  Return a >>= k  = k a
  Call x k >>= f  = Call x (k >>= f)
  GetText k >>= f = GetText $ \a -> k a >>= f
  GetLoc k >>= f  = GetLoc  $ \a -> k a >>= f

snapStep s =
  do w <- liftIO (readIORef s)
     case w of
       GetText k -> do txt <- textParam "next"
                       liftIO (writeIORef s (k txt))
       GetLoc k  -> do x <- intParam "x"
                       y <- intParam "y"
                       liftIO (writeIORef s (k (Loc x y)))
       Call cmd k -> do sendJSON cmd
                        liftIO (writeIORef s k)
       Return ()  -> return ()

data Loc  = Loc Int Int
data Cmd  = Cmd Text [Value]

instance Export Cmd where
  toJS (Cmd txt vs) = toJS (toJS txt : vs)

instance Export Loc where
  toJS (Loc x y) = object [ "x" .= x, "y" .= y ]


noRes :: Cmd -> I ()
noRes c = Call c (Return ())

cmdAppear :: Loc -> Text -> I ()
cmdAppear l t = noRes $ Cmd "appear" [ toJS l, toJS t ]

cmdDisappear :: Loc -> I ()
cmdDisappear l = noRes $ Cmd "disappear" [ toJS l ]

cmdSwapTiles :: Loc -> Loc -> I ()
cmdSwapTiles l1 l2 = noRes $ Cmd "swapTiles" [ toJS l1, toJS l2 ]

cmdMoveTile :: Loc -> Loc -> I ()
cmdMoveTile l1 l2 = noRes $ Cmd "moveTile" [ toJS l1, toJS l2 ]

cmdAddDruid :: Text -> Loc -> Text -> Bool -> Bool -> Cmd
cmdAddDruid x y z a b =
  Cmd "addDruid" [ toJS x, toJS y, toJS z, toJS a, toJS b ]

cmdRemoveDruid :: Text -> I ()
cmdRemoveDruid x = noRes $ Cmd "removeDruid" [ toJS x ]

cmdMoveDruid :: Text -> Loc -> I ()
cmdMoveDruid x y = noRes $ Cmd "moveDruid" [ toJS x, toJS y ]

cmdSetDruidState :: Text -> Bool -> I ()
cmdSetDruidState x y = noRes $ Cmd "setDruidState" [ toJS x, toJS y ]

cmdChooseTile :: [Loc] -> I Loc
cmdChooseTile xs = Call (Cmd "chooseTile" [toJS xs]) $ GetLoc Return

cmdChooseDruid :: [Text] -> I Text
cmdChooseDruid xs = Call (Cmd "chooseDruid" [toJS xs]) $ GetText Return

cmdChooseNewLocation :: [Text] -> I Text
cmdChooseNewLocation xs =
  Call (Cmd "chooseNewLocation" [toJS xs]) $ GetText Return



--------------------------------------------------------------------------------

badInput :: ByteString -> Snap a
badInput msg =
  Snap.finishWith (Snap.setResponseStatus 400 msg Snap.emptyResponse)

notFound :: Snap a
notFound = Snap.finishWith (Snap.setResponseStatus 404 "Not Found"
                                                      Snap.emptyResponse)

--------------------------------------------------------------------------------

requiredParam :: ByteString -> Snap ByteString
requiredParam p =
  do mb <- Snap.getParam p
     case mb of
       Just x  -> return x
       Nothing -> badInput ("Missing parameter: " `BS.append` p)

textParam :: ByteString -> Snap Text
textParam p = decodeUtf8 `fmap` requiredParam p

intParam :: ByteString -> Snap Int
intParam p =
  do txt <- textParam p
     let (neg,numTxt) = case Text.uncons txt of
                          Just ('-',t) -> (negate, t)
                          _            -> (id, txt)
     case decimal numTxt of
       Right (a,t) | Text.null t -> return (neg a)
       _ -> badInput ("Malformed integer parameter: " `BS.append` p)

natParam :: ByteString -> Snap Int
natParam p =
  do txt <- textParam p
     case decimal txt of
       Right (a,t) | Text.null t -> return a
       _ -> badInput ("Malformed natural parameter: " `BS.append` p)

boolParam :: ByteString -> Snap Bool
boolParam p =
  do bs <- requiredParam p
     case bs of
       "true"  -> return True
       "false" -> return False
       _       -> badInput ("Malformed boolean parameter: " `BS.append` p)



sendJSON :: Export a => a -> Snap ()
sendJSON a =
  do Snap.modifyResponse (Snap.setHeader "content-type" "application/json")
     Snap.writeLBS (jsonBytes a)


