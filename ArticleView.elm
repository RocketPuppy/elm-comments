module ArticleView (Action, State, step, render, init, actions, noop, toTitle, loadArticle, titlesTransition) where

import Debug
import Graphics.Input as I
import Maybe

import Html
import Html (text, Html)
import Html.Tags (..)
import Html.Attributes as A
import Html.Events (..)

import Utils (..)
import Json
import Http
import Dict
import Maybe (Maybe (..), isJust)

data CommentBlock = CommentBlock { collapsed : Bool, comments : [Comment] }
data Paragraph = Paragraph String CommentBlock
data Comment = Comment Author String
data Article = Article Author Title [Paragraph]
data Title = Title String
data Author = Author String

data Action = Noop | ViewTitles | ToggleComments CommentBlock | LoadArticle Title | ViewArticle (Maybe Article)
type State b = { article : Maybe Article, actionH : I.Handle b, actionFn : (Action -> b) }

actions : Signal b -> (b -> Action) -> (Action -> b) -> Signal b
actions s toA fromA = merges [s, fromA <~ (getArticle (keepIf isLoadArticle Noop (toA <~ s)))]

isLoadArticle a = case a of
    LoadArticle _ -> True
    _ -> False

noop = Noop
toTitle s = Title s
loadArticle = LoadArticle

titlesTransition a = case a of
    ViewTitles -> True
    _ -> False

init : (Action -> b) -> I.Handle b -> State b
init f h = { article = Nothing, actionH = h, actionFn = f }

step : Action -> State b -> State b
step a s =
    let a' = s.article
    in case a of
        ViewTitles -> s
        ToggleComments cb -> { s | article <- Maybe.map (toggleComments cb) a' }
        ViewArticle a -> { s | article <- a }
        LoadArticle t -> s
        Noop -> s

render : State b -> Html
render s =
    let article = Maybe.maybe (text "failed") (mkArticle s.actionH s.actionFn) s.article
        back = div [onclick s.actionH (s.actionFn << (\_ -> ViewTitles))] [text "Back To Articles"]
    in div [] [back, article]

toggleComments cb article = case article of
    Article a t ps -> Article a t (map (toggleCommentBlocks cb) ps)

toggleCommentBlocks cb p = case p of
    Paragraph s cb' -> if cb == cb' then Paragraph s (toggleCB cb) else Paragraph s cb'

toggleCB cb = case cb of
    CommentBlock cb -> CommentBlock { cb | collapsed <- not cb.collapsed }

-- rendering

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

-- network stuff

articleUrl = "http://localhost:3000/article?title="

getArticle : Signal Action -> Signal Action
getArticle actions =
    let string a = case a of
            LoadArticle (Title t) -> articleUrl ++ subSpaces t
            _ -> ""
    in (ViewArticle) <~ (httpArticle <| string <~ actions)

toATitle json = case json of
    Json.String s -> Title s

parseArticle : String -> Maybe Article
parseArticle string = Maybe.map toArticle << Json.fromString <| string

toArticle json = case json of
    Json.Object obj -> Article (Author <| jsonString (Dict.getOrFail "articleAuthor" obj)) (toATitle (Dict.getOrFail "title" obj)) (toParagraphs (Dict.getOrFail "paragraphs" obj))

toParagraphs json = case json of
    Json.Array ps -> map toParagraph ps

toParagraph json =
    let toComments json = case json of
            Json.Array cs -> map toComment cs
    in case json of
        Json.Object obj -> Paragraph (jsonString (Dict.getOrFail "paragraph" obj)) (CommentBlock { collapsed = True, comments = (toComments (Dict.getOrFail "comments" obj)) })

toComment json = case json of
    Json.Object obj -> Comment (Author <| jsonString <| Dict.getOrFail "author" obj) (jsonString <| Dict.getOrFail "message" obj)

httpArticle : Signal String -> Signal (Maybe Article)
httpArticle strings = lift (joinMaybe << Maybe.map parseArticle << extractString) <| Http.sendGet strings
