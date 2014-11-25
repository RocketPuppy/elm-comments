module Comments where

import ArticleView
import TitlesView

import Types (..)
import Router (initRouter)

import Html (Html)
import Html

import Window

router = initRouter [TitlesView.testRoute, ArticleView.testRoute]

main = render Window.dimensions router

render : Signal (Int, Int) -> Signal Html -> Signal Element
render dims htmls = (\(x, y) html -> Html.toElement x y html) <~ dims ~ htmls
