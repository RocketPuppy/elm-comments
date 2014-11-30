module ArticleView (Article(..), Author(..), Paragraph(..), Comment(..), CommentBlock(..), Title(..), Action(..), State, step, render, init) where

import Graphics.Input as I
import Maybe

import Html
import Html (text, Html)
import Html.Tags (..)
import Html.Attributes as A
import Html.Events (..)

data CommentBlock = CommentBlock { collapsed : Bool, comments : [Comment] }
data Paragraph = Paragraph String CommentBlock
data Comment = Comment Author String
data Article = Article Author Title [Paragraph]
data Title = Title String
data Author = Author String

data Action = ViewTitles | ToggleComments CommentBlock | ViewArticle (Maybe Article)
type State b = { article : Maybe Article, actionH : I.Handle b, actionFn : (Action -> b) }

init : (Action -> b) -> I.Handle b -> State b
init f h = { article = Nothing, actionH = h, actionFn = f }

step : Action -> State b -> State b
step a s =
    let a' = s.article
    in case a of
        ViewTitles -> s
        ToggleComments cb -> { s | article <- Maybe.map (toggleComments cb) a' }
        ViewArticle a -> { s | article <- a }

render : State b -> Html
render s = Maybe.maybe (text "failed") (mkArticle s.actionH s.actionFn) s.article

toggleComments cb article = case article of
    Article a t ps -> Article a t (map (foo cb) ps)

toggleCB cb = case cb of
    CommentBlock cb -> CommentBlock { cb | collapsed <- not cb.collapsed }

foo cb p = case p of
    Paragraph s cb' -> if cb == cb' then Paragraph s (toggleCB cb) else Paragraph s cb'

mkArticle : I.Handle b -> (Action -> b) -> Article -> Html
mkArticle h f a = case a of
    Article author title ps -> article [] <| [mkTitle title] ++ map (mkParagraph h f) ps ++ [mkAuthor author]

mkTitle : Title -> Html
mkTitle t = case t of
    Title title -> header [] [text title]

mkAuthor : Author -> Html
mkAuthor a = case a of
    Author author -> footer [] [text author]

mkParagraph : I.Handle b -> (Action -> b) -> Paragraph -> Html
mkParagraph h f para = case para of
    Paragraph para comments -> div [A.class "paragraph"] <| (p [] [text para])::[(mkCommentBlock h f)comments]

mkCommentBlock : I.Handle b -> (Action -> b) -> CommentBlock -> Html
mkCommentBlock h f cb = case cb of
    CommentBlock comments ->
        let className = if comments.collapsed then "comment-block collapsed" else "comment-block"
        in
        div [A.class className, onclick h (f << (\_ -> ToggleComments cb))] <| (span [A.class "button"] [text "+"])::map mkComment comments.comments

mkComment : Comment -> Html
mkComment c = case c of
    Comment author comment -> div [A.class "comment"] <| (p [] [text comment])::[mkAuthor author]
