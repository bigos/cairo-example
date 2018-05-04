  {-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GI.Gtk as GI (init,
                               main)
import GI.Gtk (mainQuit,
               onWidgetDestroy,
               windowNew,
               widgetShowAll)

import GI.Gtk.Enums (WindowType(..))

main :: IO ()
main = do
  _ <- GI.init Nothing

  win <- windowNew WindowTypeToplevel
  _ <- onWidgetDestroy win mainQuit
  widgetShowAll win
  GI.main
