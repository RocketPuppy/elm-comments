module Comments (State, step, init, render, Action) where

import TitlesView
import ArticleView

import Debug
import Graphics.Input as I
import Network
import Html (Html)
import Html

data Action = Noop | TitlesAction TitlesView.Action | ArticleAction ArticleView.Action
type State = { mostRecentAction : Action, titlesView : TitlesView.State Action, articleView : ArticleView.State Action }

step : Action -> State -> State
step a s =
    let s' = { s | mostRecentAction <- a }
    in case a of
        TitlesAction a -> { s' | titlesView <- TitlesView.step a s.titlesView }
        ArticleAction a -> { s' | articleView <- ArticleView.step a s.articleView }
        Noop -> s

initialAction = TitlesAction (TitlesView.refreshAction [])

init : (Signal Action, State)
init =
    let actionInput = I.input initialAction
    in (merges [actionInput.signal, httpTitles, httpArticle actionInput.signal],
           { mostRecentAction = initialAction
           , titlesView = TitlesView.init TitlesAction actionInput.handle
           , articleView = ArticleView.init ArticleAction actionInput.handle
           }
       )

render : State -> Html
render s = case s.mostRecentAction of
    TitlesAction _ -> TitlesView.render s.titlesView
    ArticleAction _ -> ArticleView.render s.articleView

httpTitles : Signal Action
httpTitles = (TitlesAction << TitlesView.refreshAction) <~ Network.getTitles

httpArticle : Signal Action -> Signal Action
httpArticle actions =
    let extractString a = case a of
            TitlesAction (TitlesView.ViewArticle (TitlesView.Title t)) -> Just t
            _ -> Nothing
    in (ArticleAction << ArticleView.ViewArticle) <~ (Network.getArticle <| extractString <~ actions)
