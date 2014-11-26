module TitlesView  where

import Types (..)

import Html
import Html (text, Html)
import Html.Tags (..)
import Html.Attributes as A
import Html.Events (..)

import Graphics.Input as I

mkTitle : Title -> Html
mkTitle t = case t of
    Title title -> h1 [A.class "title"] [text title]

mkTitles : I.Handle Route -> [Title] -> Html
mkTitles routeHandle titles = div [A.class "title-block", onclick routeHandle (\_ -> ArticleRoute)] <| map mkTitle titles

titles : [Title]
titles = [Title "Title 1", Title "Title 2"]

onTitles : Signal Route -> Signal Bool
onTitles signal =
    let f s = case s of
        TitleRoute -> True
        _ -> False
    in f <~ signal

render : Signal [Title] -> Renderer
render titles handle = mkTitles handle <~ titles

testRender = render (constant titles)
