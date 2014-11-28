module Network where

import Types (..)

import Http
import Json
import Maybe (Maybe (..), isJust)
import Maybe
import Dict
import String

import Graphics.Input as I

parseTitle : String -> Maybe [Title]
parseTitle string =
    let toTitles json = case json of
            Json.Array vs -> map toTitle <| vs
            _ -> []
    in (Maybe.map toTitles) << Json.fromString <| string

toTitle json = case json of
    Json.String s -> Title s

getTitles : Signal [Title]
getTitles = lift (Maybe.maybe [] identity << joinMaybe << Maybe.map parseTitle << extractString) <| Http.sendGet <| constant "http://localhost:3000/articles"

parseArticle : String -> Maybe Article
parseArticle string = Maybe.map toArticle << Json.fromString <| string

toArticle json = case json of
    Json.Object obj -> Article (Author <| jsonString (Dict.getOrFail "articleAuthor" obj)) (toTitle (Dict.getOrFail "title" obj)) (toParagraphs (Dict.getOrFail "paragraphs" obj))

toParagraphs json = case json of
    Json.Array ps -> map toParagraph ps

toParagraph json =
    let toComments json = case json of
            Json.Array cs -> map toComment cs
    in case json of
        Json.Object obj -> Paragraph (jsonString (Dict.getOrFail "paragraph" obj)) (CommentBlock { collapsed = True, comments = (toComments (Dict.getOrFail "comments" obj)), input = I.input True })

jsonString json = case json of
    Json.String s -> s

toComment json = case json of
    Json.Object obj -> Comment (Author <| jsonString <| Dict.getOrFail "author" obj) (jsonString <| Dict.getOrFail "message" obj)

toPlus c = if c == ' ' then '+' else c

getArticle : Signal Title -> Signal (Maybe Article)
getArticle titles = lift (joinMaybe << Maybe.map parseArticle << extractString) <| Http.sendGet <| lift ((\t -> "http://localhost:3000/article?title=" ++ t) << (\t -> String.map toPlus t) << (\t -> case t of Title t -> t)) titles

fromJust j = case j of
    Just v -> v

extractString r = case r of
    Http.Success v -> Just v
    _ -> Nothing

isSuccess r = case r of
    Http.Success _ -> True
    _ -> False

joinMaybe : Maybe (Maybe a) -> Maybe a
joinMaybe m = case m of
    Just m' -> m'
    _ -> Nothing
