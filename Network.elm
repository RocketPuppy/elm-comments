module Network where

import TitlesView (Title)
import TitlesView
import ArticleView (Article(..), Author(..), Paragraph(..), Comment(..), CommentBlock(..))
import ArticleView

import Http
import Maybe (Maybe (..), isJust)
import Maybe
import Dict
import String
import Json

import Graphics.Input as I

parseTitles : Json.Value -> [Title]
parseTitles json = case json of
    Json.Array vs -> map toTTitle vs
    _ -> []

toTTitle json = case json of
    Json.String s -> TitlesView.Title s

toATitle json = case json of
    Json.String s -> ArticleView.Title s

joinMaybe : Maybe (Maybe a) -> Maybe a
joinMaybe m = case m of
    Just m' -> m'
    _ -> Nothing

getTitles : Signal [Title]
getTitles = lift (Maybe.maybe [] parseTitles << joinMaybe << Maybe.map Json.fromString << extractString) <| Http.sendGet <| constant "http://localhost:3000/articles"

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

jsonString json = case json of
    Json.String s -> s

toComment json = case json of
    Json.Object obj -> Comment (Author <| jsonString <| Dict.getOrFail "author" obj) (jsonString <| Dict.getOrFail "message" obj)

toPlus c = if c == ' ' then '+' else c

getArticle : Signal (Maybe String) -> Signal (Maybe Article)
getArticle titles = lift (joinMaybe << Maybe.map parseArticle << extractString) <| Http.sendGet <| lift fromJust <| keepIf isJust (Just "") <| (Maybe.map addArticleUrl << Maybe.map subSpaces) <~ titles

addArticleUrl t = "http://localhost:3000/article?title=" ++ t
subSpaces t = String.map toPlus t

fromJust j = case j of
    Just v -> v

extractString r = case r of
    Http.Success v -> Just v
    _ -> Nothing

isSuccess r = case r of
    Http.Success _ -> True
    _ -> False
