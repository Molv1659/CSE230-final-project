module Main where

import Brick 
    (App(..), attrMap
    )
import Brick.BChan
import Control.Monad (void, forever)
import Control.Concurrent (threadDelay, forkIO)
import qualified Brick.Main as M
import UI
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

    chan <- newBChan 10

    void $ forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 1000000
        
    initialVty <- buildVty
    void $ M.customMain initialVty buildVty (Just chan) app $ getInitialState
