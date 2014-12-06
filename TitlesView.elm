module TitlesView (Action, State, step, render, init, actions, noop, refresh, fromTitle, articleTransition) where

import Utils

import Html
import Html (text, Html)
import Html.Tags (..)
import Html.Attributes as A
import Html.Events (..)
import Graphics.Input as I

import Json
import Http
import Maybe (Maybe (..), isJust)
import Maybe

import Debug

data Title = Title String

data Action = Noop | LoadTitles [Title] | Refresh | ViewArticle Title
type State b = { titles : [Title], actionH : I.Handle b, actionFn : (Action -> b) }

actions : Signal b -> (b -> Action) -> (Action -> b) -> Signal b
actions s toA fromA = merges [s, fromA <~ (getTitles (keepIf (\a -> a == Refresh) Noop (toA <~ s)))]

noop = Noop
refresh = Refresh
fromTitle title = case title of
    Title t -> t
articleTransition a = case a of
    ViewArticle t -> Just t
    _ -> Nothing

init : (Action -> b) -> I.Handle b -> State b
init f h = { titles = [], actionH = h, actionFn = f }

step : Action -> State b -> State b
step a s = case a of
    LoadTitles ts -> { s | titles <- ts }
    -- side effecting actions
    ViewArticle _ -> s
    Refresh -> s
    -- doesn't do anything
    Noop -> s

-- rendering

mkTitle : I.Handle b -> (Action -> b) -> Title -> Html
mkTitle h f t = case t of
    Title title -> h1 [A.class "title", onclick h (f << (\_ -> ViewArticle t))] [text title]

render : State b -> Html
render s = div [A.class "title-block"] <| map (mkTitle s.actionH s.actionFn) s.titles

-- over the wire stuff

parseTitles : Json.Value -> [Title]
parseTitles json = case json of
    Json.Array vs -> map toTitle vs
    _ -> []

toTitle json = case json of
    Json.String s -> Title s

titlesURL = "http://localhost:3000/articles"

getTitles : Signal Action -> Signal Action
getTitles actions =
    let strings a = case a of
            Refresh -> titlesURL
            _ -> ""
        fn s = LoadTitles << Maybe.maybe [] parseTitles << Utils.joinMaybe << Maybe.map Json.fromString << Utils.extractString <~ Http.sendGet s
    in fn (strings <~ actions)
