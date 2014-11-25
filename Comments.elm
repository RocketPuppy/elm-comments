module Comments where

import ArticleView
import TitlesView

import Types (..)

import Html (Html)

import Window

main = render Window.dimensions (router [TitlesView.testRoute, ArticleView.testRoute])

render : Signal (Int, Int) -> Signal Html -> Signal Element
render dims htmls = (\(x, y) html -> Html.toElement x y html) <~ dims ~ htmls

router : [(Signal Bool, Signal Html)] -> Signal Html
router signals =
    let f (p, s) = sampleOn (keepWhen p 0 (merge (count s) (count p))) s
    in merges << map f <| signals
