{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.IORef (newIORef, readIORef, modifyIORef')
import Control.Monad.Trans.Reader (runReaderT)
import           Foreign.Ptr (castPtr)
import qualified GI.Cairo as GICairo
-- import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
-- import Data.GI.Base.ManagedPtr

import GI.Gtk.Enums (WindowType(..))

type LastKey = Integer
data Heading = Hleft | Hup | Hright | Hdown | None deriving (Eq, Show)

data Model = Model { lastKey :: LastKey
                   , heading :: Heading
                   } deriving (Show)

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GICairo.Context -> Render () -> IO ()
renderWithContext ct r = Gdk.withManagedPtr ct $ \p ->
  runReaderT (runRender r) (Cairo (castPtr p))

updateCanvas :: Gtk.DrawingArea -> Model -> Render ()
updateCanvas canvas model = do
  width'  <- fromIntegral <$> Gtk.widgetGetAllocatedWidth canvas
  height' <- fromIntegral <$> Gtk.widgetGetAllocatedHeight canvas
  let mwidth  = realToFrac width'
      mheight = realToFrac height'

  -- drawing changes color when you press 'a' on keyboard
  if (lastKey model) == 97 then setSourceRGB 0.9 0.5 0 else setSourceRGB 0.6 0.9 0
  setLineWidth 20
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  moveTo (mheight/2) (mwidth/2)
  case (heading model) of Hleft ->  lineTo (0+offset) (mheight/2)
                          Hup ->    lineTo (mwidth/2) (0+offset)
                          Hright -> lineTo ((mheight)-offset) (mwidth/2)
                          Hdown ->  lineTo (mheight/2) ((mwidth)-offset)
                          None ->   lineTo (mheight/2) (mwidth/2)
  stroke
  where offset = 30

keyToHeading :: LastKey -> Heading
keyToHeading lk
  | lk == 65361 = Hleft
  | lk == 65362 = Hup
  | lk == 65363 = Hright
  | lk == 65364 = Hdown
  | otherwise = None

ifNoneThen :: Heading -> Heading -> Heading
None `ifNoneThen` x = x
h    `ifNoneThen` _ = h

updateModel :: Integer -> Model -> Model
updateModel kv oldModel = Model newKv newHeading
    where newKv      = fromIntegral kv
          newHeading = keyToHeading newKv `ifNoneThen` heading oldModel

main = do
  _ <- Gtk.init  Nothing

  globalModel <- newIORef ((Model 0 Hright) :: Model)

  win <- Gtk.windowNew WindowTypeToplevel
  canvas <- Gtk.drawingAreaNew
  Gtk.containerAdd win canvas

  _ <- Gtk.onWidgetDraw canvas $ \context ->
    putStrLn ("drawing event ") >>
    readIORef globalModel >>=
    (\model ->
    (renderWithContext context (updateCanvas canvas model))) >> pure True

  _ <- Gtk.onWidgetKeyPressEvent win $ \rkv -> do
    kv <- Gdk.getEventKeyKeyval rkv

    -- update globalModel in place
    modifyIORef' globalModel (updateModel (fromIntegral kv))

    -- this forces redrawing of canvas widget
    Gtk.widgetQueueDraw canvas
    readIORef globalModel >>=
      (\ov ->
         (putStrLn ( "You have pressed key code"  ++ (show ov))))  >> pure True

  _ <- Gtk.onWidgetDestroy win Gtk.mainQuit

  Gtk.widgetShowAll win
  Gtk.main
