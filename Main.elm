module Main where

import Comments

import Window
import Html

main = scene Window.dimensions state

state : Signal Comments.State
state =
    let (actions, initial) = Comments.init
    in foldp Comments.step initial actions

scene : Signal (Int, Int) -> Signal Comments.State -> Signal Element
scene dims s = (\(x, y) html -> Html.toElement x y html) <~ dims ~ (Comments.render <~ s)
