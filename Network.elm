module Network where

import TitlesView (parseTitle)
import ArticleView (Article(..), Author(..), Paragraph(..), Comment(..), CommentBlock(..))
import ArticleView

import Http
import Maybe (Maybe (..), isJust)
import Maybe
import Dict
import String
import Json

import Graphics.Input as I

toATitle json = case json of
    Json.String s -> ArticleView.Title s

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

getArticle : Signal (Maybe String) -> Signal (Maybe Article)
getArticle titles = lift (joinMaybe << Maybe.map parseArticle << extractString) <| Http.sendGet <| lift fromJust <| keepIf isJust (Just "") <| (Maybe.map addArticleUrl << Maybe.map subSpaces) <~ titles

addArticleUrl t = "http://localhost:3000/article?title=" ++ t
