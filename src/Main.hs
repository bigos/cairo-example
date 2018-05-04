  {-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GI.Gtk as GI (init,
                               main)
import GI.Gtk (
  containerAdd
  , drawingAreaNew
  , onWidgetDestroy
  , widgetShowAll
  , windowNew
  , mainQuit
  )

import GI.Cairo
--import Graphics.Rendering.Cairo

import GI.Gtk.Enums (WindowType(..))



main :: IO ()
main = do
  _ <- GI.init Nothing

  win <- windowNew WindowTypeToplevel

  clock <- drawingAreaNew
  containerAdd win clock

  _ <- onWidgetDestroy win mainQuit
  widgetShowAll win
  GI.main
