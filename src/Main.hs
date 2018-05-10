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
data GameField = Move | Feeding | Collision | Pause deriving (Show, Eq)
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
initGlobalModel = newIORef (Model { debugData = ""
                                  , eaten = 0
                                  , foodItems = []
                                  , gameField = Pause
                                  , snakeLength = 1
                                  , heading = HeadingRight
                                  , Main.height = 400 -- does Haskell use functions to find record elements?
                                  , lastKey = 32
                                  , Main.scale = 25
                                  , snake = [(6,7),(5,7)]
                                  , tickInterval = 500 -- time
                                  , Main.width = 600 })

-- in the above example Main.height is explained here with following text
-- https://en.wikibooks.org/wiki/Haskell/More_on_datatypes
-- This will automatically generate the following accessor functions for us:

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


shrink :: Int -> Int
shrink n = if (n-1) > 0 then n-1 else 0

foodUnderHead :: Coordinate -> Model -> Bool
foodUnderHead c model =
  (fst c)==cx && (snd c)==cy
  where hsm = head (snake model)
        cx = fst hsm
        cy = snd hsm

foodEaten :: Model -> Bool
foodEaten model =
  any id (map (\c -> (fst c)==cx && (snd c)==cy) (foodItems model))
  where hsm = head (snake model)
        cx = fst hsm
        cy = snd hsm

snakeGrower :: Int -> Snake -> Snake
snakeGrower growth snakecc =
  case (compare growth 0) of
    GT -> snakecc
    EQ -> init snakecc
    LT -> init $ init snakecc

moveSnake :: Model -> Heading -> Snake
moveSnake model headingv =
  if ((gameField model == Pause) || (gameField model) == Collision)
  then (snake model)
  else moveSnake2 model headingv

moveSnake2 :: Model -> Heading -> Snake
moveSnake2 model headingv =
  case headingv of
    HeadingLeft ->  (fst uhs-1, snd uhs) : snakeGrower growth snake'
    HeadingUp ->    (fst uhs, snd uhs-1) : snakeGrower growth snake'
    HeadingRight -> (fst uhs+1, snd uhs) : snakeGrower growth snake'
    HeadingDown ->  (fst uhs, snd uhs+1) : snakeGrower growth snake'
  where snake' = snake model
        growth = snakeLength model
        uhs = head snake'

updateGamefield :: (Num a, Eq a) => Bool -> Model -> a -> GameField
updateGamefield keyEvent model kk =
  if keyEvent
  then case (gameField model) of
    Pause -> Move
    Move -> if kk == 32
      then Pause
      else gameField model
    _ -> gameField model

  else gameField model

cook :: Model -> Model
cook model =
  if foodEaten model
  then model { gameField = undefined
             , snakeLength = (snakeLength model) +3
             , foodItems = filter (\c -> not (foodUnderHead c model)) (foodItems model)
             , debugData = "" --show ("** eaten ** ", head (snake model), (foodItems model))
             , eaten = (eaten model) + 1
             }
  else model { gameField = undefined
             , snakeLength = shrink (snakeLength model)
             , debugData = "" }

data Msg = Tick | Keypress LastKey | NewFood FoodItems deriving (Show)

updateGlobalModel :: Msg -> Model -> Model
updateGlobalModel (Tick) rawModel = updateTickFields model
  where model = cook rawModel
        updateTickFields m = m { gameField = updateGamefield False model (lastKey model)
                               , snake = moveSnake model (heading model) }
updateGlobalModel (Keypress kv) oldModel = updateFields oldModel
    where newKv      = fromIntegral kv
          newHeading = keyToHeading newKv `ifNoneThen` heading oldModel
          updateFields m = m {lastKey = newKv, heading = newHeading}

-- updateGlobalModel (NewFood fi) oldModel =

-- main ----------------------------------------

main :: IO ()
main = do
  _ <- Gtk.init  Nothing

  globalModel <- initGlobalModel

  win <- Gtk.windowNew WindowTypeToplevel
  canvas <- Gtk.drawingAreaNew
  Gtk.containerAdd win canvas

  _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1000 (modifyIORef' globalModel (updateGlobalModel Tick) >> Gtk.widgetQueueDraw canvas >> return True)

  _ <- Gtk.onWidgetDraw canvas $ \context ->
    getWidgetSize canvas >>= (\ss->  putStrLn ("drawing event - widget size " ++ (show ss))) >>
    readIORef globalModel >>=
    (\model ->
    (renderWithContext context (drawCanvas canvas model))) >> pure True

  _ <- Gtk.onWidgetKeyPressEvent win $ \rkv -> do
    kv <- Gdk.getEventKeyKeyval rkv

    -- update globalModel in place
    modifyIORef' globalModel (updateGlobalModel (Keypress (fromIntegral kv)))

    -- this forces redrawing of canvas widget
    Gtk.widgetQueueDraw canvas
    readIORef globalModel >>=
      (\ov ->
         (putStrLn ( "You have pressed key code"  ++ (show ov))))  >> pure True

  _ <- Gtk.onWidgetDestroy win Gtk.mainQuit

  Gtk.widgetShowAll win
  Gtk.main
