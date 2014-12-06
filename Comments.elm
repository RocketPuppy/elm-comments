module Comments (State, step, init, render, Action) where

import TitlesView
import ArticleView

import Debug
import Graphics.Input as I
import Html (Html)
import Html

data Action = Noop | TitlesAction TitlesView.Action | ArticleAction ArticleView.Action
type State = { mostRecentAction : Action, titlesView : TitlesView.State Action, articleView : ArticleView.State Action }

step : Action -> State -> State
step a s =
    let s' = { s | mostRecentAction <- a }
    in case a of
        TitlesAction a -> { s' | titlesView <- TitlesView.step a s'.titlesView }
        ArticleAction a -> { s' | articleView <- ArticleView.step a s'.articleView }
        Noop -> s

toTitlesViewAction a = case a of
    TitlesAction a -> a
    _ -> TitlesView.noop

isTitlesAction a = case a of
    TitlesAction _ -> True
    _ -> False

toArticleViewAction a = case a of
    ArticleAction a -> a
    _ -> ArticleView.noop

isArticleAction a = case a of
    ArticleAction _ -> True
    _ -> False

initialInput = TitlesAction (TitlesView.refresh)

titles2Article : TitlesView.Action -> Action
titles2Article a = case TitlesView.articleTransition a of
    Just t -> ArticleAction << ArticleView.loadArticle << ArticleView.toTitle << TitlesView.fromTitle <| t
    Nothing -> TitlesAction a

article2Titles : ArticleView.Action -> Action
article2Titles a = if ArticleView.titlesTransition a
                      then TitlesAction TitlesView.refresh
                      else ArticleAction a

init : (Signal Action, State)
init =
    let actionInput = I.input initialInput
        actions = Debug.watch "comments.action" <~ merges [Debug.watch "titlesActions'" <~ titlesActions', Debug.watch "articleActions'" <~ articleActions', Debug.watch "actionInput.signal" <~ actionInput.signal]
        mkComponentSignal s keep init to from actions = s (keepIf keep init actions) to from
        titlesActions a = titles2Article << toTitlesViewAction <~ mkComponentSignal TitlesView.actions isTitlesAction (TitlesAction TitlesView.refresh) toTitlesViewAction TitlesAction a
        articleActions a = article2Titles << toArticleViewAction << Debug.watch "articleActions" <~ mkComponentSignal ArticleView.actions isArticleAction (ArticleAction ArticleView.noop) toArticleViewAction ArticleAction a
        articleActions' = articleActions (titlesActions actionInput.signal)
        titlesActions' = titlesActions (articleActions actionInput.signal)
    in (actions,
           { mostRecentAction = initialInput
           , titlesView = TitlesView.init TitlesAction actionInput.handle
           , articleView = ArticleView.init ArticleAction actionInput.handle
           }
       )

render : State -> Html
render s = case s.mostRecentAction of
    TitlesAction _ -> TitlesView.render s.titlesView
    ArticleAction _ -> ArticleView.render s.articleView
    Noop -> Html.text "noop"
