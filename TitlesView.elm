module TitlesView  where

import Types (..)

import Html
import Html (text, Html)
import Html.Tags (..)
import Html.Attributes as A
import Html.Events (..)

mkTitle : Title -> Html
mkTitle t = case t of
    Title title -> h1 [A.class "title"] [text title]

mkTitles : [Title] -> Html
mkTitles titles = div [A.class "title-block", onclick screenInput.handle (\_ -> ArticleScreen)] <| map mkTitle titles

titles : [Title]
titles = [Title "Title 1", Title "Title 2"]

render : Signal [Title] -> Signal Html
render titles = mkTitles <~ titles

testRender = render (constant titles)

onTitles : Signal Bool
onTitles =
    let f s = case s of
        TitleScreen -> True
        _ -> False
    in f <~ screenInput.signal

route : Signal [Title] -> (Signal Bool, Signal Html)
route s = (onTitles, render s)

testRoute = route (constant titles)
