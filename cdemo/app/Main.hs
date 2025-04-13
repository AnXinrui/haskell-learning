module Main where

import GI.Gtk hiding (Widget, Bin, (:=), Container, on, main)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

main :: IO ()
main = putStrLn "Hello, Haskell!"
