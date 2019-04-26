module Main where

import Data.Time.Clock

import Types
import Storage.Serialize


import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor, attrName, simpleMain
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U

drawInfo :: Widget ()
drawInfo = withBorderStyle BS.unicodeBold
  $ C.hCenter
  $ hLimit 80
  $ vLimit 400
  $ B.borderWithLabel (str " Welcome to Labda-Time ")
  $ vBox $ map (uncurry drawKey)
  $ [ ("d", "Delete")
    , ("s", "Show details")
    , ("m", "Modify")
    ]
    where
      drawKey act key = (padRight Max $ padLeft (Pad 1) $ str act)
                        <+> (padLeft Max $ padRight (Pad 1) $ str key)


main :: IO ()
main = do
  generateTestData

  simpleMain drawInfo


-- Generate some demo data to work with
generateTestData :: IO ()
generateTestData = do
  time <- getCurrentTime

  let e1 = (Entry {startTime = time, endTime = time, description = "Test data 1"})
  let e2 = (Entry {startTime = time, endTime = time, description = "Test data 2"})
  let e3 = (Entry {startTime = time, endTime = time, description = "Test data 3"})
  let e4 = (Entry {startTime = time, endTime = time, description = "Test data 4"})

  let l = [e1, e2, e3, e4]

  save l