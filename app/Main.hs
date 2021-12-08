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
    ,cursor
    )
import qualified Graphics.Vty as V
import Control.Monad (void)

-- ui :: Widget ()
-- ui = str "Hello, world!"

app :: App GameState Tick ResourceName
app = App {
    appDraw = drawUi,
    -- appChooseCursor = M.neverShowCursor,
    appChooseCursor = cursor,
    appHandleEvent = handleEvent,
    appStartEvent = return,
    appAttrMap = const $ attrMap V.defAttr []
}


main :: IO ()
main = do
    -- enable mouse event
    let buildVty = do {
        v <- V.mkVty =<< V.standardIOConfig;
        V.setMode (V.outputIface v) V.Mouse True;
        return v;
    }
        
    initialVty <- buildVty
    void $ M.customMain initialVty buildVty Nothing app $ getInitialState
