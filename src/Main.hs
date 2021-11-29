module Main where

import Brick 
    (App(..), attrMap
    )
import qualified Brick.Main as M
import BoardState as BS
import UI 
    (Game, ResourceName
    ,drawUi
    ,handleEvent
    )
import qualified Graphics.Vty as V
import Control.Monad (void)

-- ui :: Widget ()
-- ui = str "Hello, world!"

app :: App Game Tick ResourceName
app = App {
    appDraw = drawUi,
    appChooseCursor = M.neverShowCursor,
    appHandleEvent = handleEvent,
    appStartEvent = return,
    appAttrMap = const $ attrMap V.defAttr []
}

initState :: BoardState
initState = Board { 
    _size=19,  -- size of the board, need to be updated in appStartEvent
    _round=0,   -- how many rounds have passed
    _black=[],  -- locations of black
    _white=[]  -- locations of white
}

main :: IO ()
main = void $ M.defaultMain app initState
