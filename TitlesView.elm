module TitlesView (Title(..), Action(..), State, step, render, init, refreshAction) where

import Html
import Html (text, Html)
import Html.Tags (..)
import Html.Attributes as A
import Html.Events (..)
import Graphics.Input as I

data Title = Title String

data Action = Refresh [Title] | ViewArticle Title
type State b = { titles : [Title], actionH : I.Handle b, actionFn : (Action -> b) }

refreshAction = Refresh

init : (Action -> b) -> I.Handle b -> State b
init f h = { titles = [], actionH = h, actionFn = f }

step : Action -> State b -> State b
step a s = case a of
    Refresh ts -> { s | titles <- ts }
    ViewArticle _ -> s

mkTitle : I.Handle b -> (Action -> b) -> Title -> Html
mkTitle h f t = case t of
    Title title -> h1 [A.class "title", onclick h (f << (\_ -> ViewArticle t))] [text title]

render : State b -> Html
render s = div [A.class "title-block"] <| map (mkTitle s.actionH s.actionFn) s.titles
