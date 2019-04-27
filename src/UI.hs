{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module UI (main) where

import qualified Types as LT

import Lens.Micro ((^.))
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)

drawUI :: L.List () LT.Entry -> [Widget ()]
drawUI l = [ui]
    where
        label = str "Item " <+> cur <+> str " of " <+> total
        cur = case l^.(L.listSelectedL) of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.(L.listElementsL)
        box = B.borderWithLabel label $
              L.renderList listDrawElement True l
        ui = C.vCenter $ vBox [ C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press Esc to exit."
                              ]

appEvent :: L.List () LT.Entry -> T.BrickEvent () e -> T.EventM () (T.Next (L.List () LT.Entry))
appEvent l (T.VtyEvent e) =
    case e of        
        V.EvKey V.KEsc [] -> M.halt l

        ev -> M.continue =<< L.handleListEvent ev l
appEvent l _ = M.continue l

listDrawElement :: Bool -> LT.Entry -> Widget ()
listDrawElement sel entry = C.hCenter $ str $ show ( LT.description entry)

initialState :: LT.Log -> L.List () LT.Entry
initialState l = L.list () (Vec.fromList l) 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            fg V.white)
    , (L.listSelectedAttr,    V.white `on` V.blue)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App (L.List () LT.Entry) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: LT.Log -> IO ()
main log = void $ M.defaultMain theApp (initialState log)