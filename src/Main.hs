{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.IORef (newIORef, readIORef, writeIORef)
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

data Model = Model { lastKey :: Int
                   , heading :: Heading
                   } deriving (Show)

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GICairo.Context -> Render () -> IO ()
renderWithContext ct r = Gdk.withManagedPtr ct $ \p ->
  runReaderT (runRender r) (Cairo (castPtr p))

--updateCanvas :: Gtk.DrawingArea -> LastKey -> Render ()
updateCanvas canvas nodel = do
  width'  <- fromIntegral <$> Gtk.widgetGetAllocatedWidth canvas
  height' <- fromIntegral <$> Gtk.widgetGetAllocatedHeight canvas
  let mwidth  = realToFrac width'
      mheight = realToFrac height'
      lastkey = 97
      o = Hleft

  -- drawing changes color when you press 'a' on keyboard
  if lastkey == 97 then setSourceRGB 0.9 0.5 0 else setSourceRGB 0.6 0.9 0
  setLineWidth 20
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  moveTo (mheight/2) (mwidth/2)

  case o of Hleft ->  lineTo (mheight/2) (0)
            Hup ->    lineTo (0) (mwidth/2)
            Hright -> lineTo (mheight/2) (mwidth)
            Hdown ->  lineTo (mheight) (mwidth/2)
            otherwise -> lineTo (mheight/2) (mwidth/2)

  stroke

keyToHeading :: LastKey -> Heading
keyToHeading lk
  | lk == 65361 = Hleft
  | lk == 65362 = Hup
  | lk == 65363 = Hright
  | lk == 65364 = Hdown
  | otherwise = None

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
    -- writeIORef lastKey (fromIntegral kv)


    readIORef globalModel >>= (\m -> writeIORef globalModel (Model
                                                             (fromIntegral kv)
                                                             (if ((keyToHeading (fromIntegral kv)) == None)
                                                               then (heading m)
                                                               else (keyToHeading (fromIntegral kv)))))

    -- this forces redrawing of canvas widget
    Gtk.widgetQueueDraw canvas
    readIORef globalModel >>=
      (\ov ->
         (putStrLn ( "You have pressed key code"  ++ (show ov))))  >> pure True

  _ <- Gtk.onWidgetDestroy win Gtk.mainQuit

  Gtk.widgetShowAll win
  Gtk.main


-- monad state
-- http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html
