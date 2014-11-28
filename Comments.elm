module Comments where

import ArticleView
import TitlesView

import Types (..)
import Router (..)
import Network (..)

import Html (Html)
import Html

import Window

main = render Window.dimensions router

router = initRouter TitleRoute [(TitleRoute, TitlesView.render getTitles), (ArticleRoute, ArticleView.render (getArticle <| constant (Title "Article Title")))]

render : Signal (Int, Int) -> Signal Html -> Signal Element
render dims htmls = (\(x, y) html -> Html.toElement x y html) <~ dims ~ htmls
