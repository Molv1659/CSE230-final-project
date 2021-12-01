module Main where

import Brick 
    (App(..), attrMap
    )
import qualified Brick.Main as M
import UI 
    (GameState, ResourceName, getInitialState
    ,Tick
    ,drawUi
    ,handleEvent
    )
import qualified Graphics.Vty as V
import Control.Monad (void)

-- ui :: Widget ()
-- ui = str "Hello, world!"

app :: App GameState Tick ResourceName
app = App {
    appDraw = drawUi,
    appChooseCursor = M.neverShowCursor,
    appHandleEvent = handleEvent,
    appStartEvent = return,
    appAttrMap = const $ attrMap V.defAttr []
}


main :: IO ()
main = void $ M.defaultMain app $ getInitialState
