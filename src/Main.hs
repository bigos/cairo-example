{-# LANGUAGE OverloadedStrings #-}

module Main where

-- imports ----------------------------------------

import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Control.Monad.Trans.Reader (runReaderT)
import           Foreign.Ptr (castPtr)
import qualified GI.Cairo as GICairo
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
-- import Data.GI.Base.ManagedPtr

import GI.Gtk.Enums (WindowType(..))

-- model ----------------------------------------

type Coordinate = (Int, Int)
type FoodItems = [Coordinate]
type Snake = [Coordinate]
type LastKey = Integer
data GameField = Move | Feeding | Collision | Pause deriving (Show)
data Model = Model {
  debugData :: String
  , eaten :: Int
  , foodItems :: FoodItems
  , gameField :: GameField
  , snakeLength :: Int
  , heading :: Heading
  , height :: Int
  , lastKey :: LastKey        -- different in elm version
  , scale :: Int
  , snake :: Snake
  , tickInterval :: Float
  -- , time :: Maybe Time
  , width :: Int
  } deriving (Show)

data Heading = HeadingLeft | HeadingUp | HeadingRight | HeadingDown | None deriving (Eq, Show)
data KeyControl = KeyPause | KeyLeft | KeyUp | KeyRight | KeyDown | KeyOther

initGlobalModel :: IO (Data.IORef.IORef Model)
initGlobalModel = newIORef (Model
                            ""  -- debugData
                           0  -- eaten
                           []  -- foodItems
                           Pause -- gameField
                           1  -- snakeLength
                           HeadingRight  -- heading
                           400  -- height
                           32  -- lastKey
                           25  -- scale
                           [(6,7),(5,7)]  -- snake
                           500  -- tickInterval
                           -- -- time
                           600  -- width
                           )
-- helpers ----------------------------------------

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GICairo.Context -> Render () -> IO ()
renderWithContext ct r = Gdk.withManagedPtr ct $ \p ->
  runReaderT (runRender r) (Cairo (castPtr p))

getWidgetSize :: Gtk.DrawingArea -> IO (Int, Int)
getWidgetSize widget = do
  width'  <- fromIntegral <$> Gtk.widgetGetAllocatedWidth widget
  height' <- fromIntegral <$> Gtk.widgetGetAllocatedHeight widget
  return (width', height')

keyToHeading :: LastKey -> Heading
keyToHeading lk
  | lk == 65361 = HeadingLeft
  | lk == 65362 = HeadingUp
  | lk == 65363 = HeadingRight
  | lk == 65364 = HeadingDown
  | otherwise = None

ifNoneThen :: Heading -> Heading -> Heading
None `ifNoneThen` v = v
h    `ifNoneThen` _ = h

-- view ----------------------------------------

drawCanvas :: Gtk.DrawingArea -> Model -> Render ()
drawCanvas canvas model = do
  size <- liftIO (getWidgetSize canvas)
  let mwidth  = fromIntegral (fst size)
      mheight = fromIntegral (snd size)

  -- drawing changes color when you press 'a' on keyboard
  if (lastKey model) == 97 then setSourceRGB 0.9 0.5 0 else setSourceRGB 0.6 0.9 0
  setLineWidth 20
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  moveTo (mwidth / 2) (mheight / 2)
  case (heading model) of HeadingLeft ->  lineTo (0+offset) (mheight/2)
                          HeadingUp ->    lineTo (mwidth/2) (0+offset)
                          HeadingRight -> lineTo (mwidth-offset) (mheight/2)
                          HeadingDown ->  lineTo (mwidth/2) (mheight-offset)
                          None ->         lineTo (mwidth / 2) (mheight / 2)
  stroke
  where offset = 30

-- update ----------------------------------------

updateGlobalModel :: Integer -> Model -> Model
updateGlobalModel kv oldModel = updateFields oldModel
    where newKv      = fromIntegral kv
          newHeading = keyToHeading newKv `ifNoneThen` heading oldModel
          updateFields m = m {lastKey = newKv, heading = newHeading}

-- main ----------------------------------------

timeToMove = do
  putStrLn "move on"

main :: IO ()
main = do
  _ <- Gtk.init  Nothing

  globalModel <- initGlobalModel

  win <- Gtk.windowNew WindowTypeToplevel
  canvas <- Gtk.drawingAreaNew
  Gtk.containerAdd win canvas

  _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1000 (timeToMove >> Gtk.widgetQueueDraw canvas >> return True)

  _ <- Gtk.onWidgetDraw canvas $ \context ->
    getWidgetSize canvas >>= (\ss->  putStrLn ("drawing event - widget size " ++ (show ss))) >>
    readIORef globalModel >>=
    (\model ->
    (renderWithContext context (drawCanvas canvas model))) >> pure True

  _ <- Gtk.onWidgetKeyPressEvent win $ \rkv -> do
    kv <- Gdk.getEventKeyKeyval rkv

    -- update globalModel in place
    modifyIORef' globalModel (updateGlobalModel (fromIntegral kv))

    -- this forces redrawing of canvas widget
    Gtk.widgetQueueDraw canvas
    readIORef globalModel >>=
      (\ov ->
         (putStrLn ( "You have pressed key code"  ++ (show ov))))  >> pure True

  _ <- Gtk.onWidgetDestroy win Gtk.mainQuit

  Gtk.widgetShowAll win
  Gtk.main
