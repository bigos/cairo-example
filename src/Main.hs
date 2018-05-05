{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Reader (runReaderT)
import           Foreign.Ptr (castPtr)
import qualified GI.Cairo as GICairo
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Data.GI.Base.ManagedPtr

import GI.Gtk.Enums (WindowType(..))

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GICairo.Context -> Render () -> IO ()
renderWithContext ct r = Gdk.withManagedPtr ct $ \p ->
  runReaderT (runRender r) (Cairo (castPtr p))

updateCanvas :: Gtk.DrawingArea -> Render ()
updateCanvas canvas = do
  width'  <- fromIntegral <$> Gtk.widgetGetAllocatedWidth canvas
  height' <- fromIntegral <$> Gtk.widgetGetAllocatedHeight canvas
  let mwidth  = realToFrac width'
      mheight = realToFrac height'

  setSourceRGB 0.6 0.9 0
  setLineWidth 20
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  moveTo 30 30
  lineTo (mwidth-30) (mheight-30)
  lineTo (mwidth-30) 30
  lineTo 30 (mheight-30)
  stroke

main = do
  _ <- Gtk.init  Nothing

  win <- Gtk.windowNew WindowTypeToplevel
  canvas <- Gtk.drawingAreaNew
  Gtk.containerAdd win canvas

  _ <- Gtk.onWidgetDraw canvas $ \context ->
    renderWithContext context (updateCanvas canvas) >> pure True

  _ <- Gtk.onWidgetKeyPressEvent win $ \x -> do
    vvv <- Gdk.getEventKeyKeyval x
    -- How do I draw on canvas here?
    (putStrLn ("You have pressed key code " ++  (show vvv))) >> pure True

  _ <- Gtk.onWidgetDestroy win Gtk.mainQuit

  Gtk.widgetShowAll win
  Gtk.main
