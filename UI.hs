{-# LANGUAGE OverloadedStrings #-}
module UI where

import JSON(remote,I)
import Board(Loc)
import Data.Text ( Text )

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




