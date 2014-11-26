module Network where

import Types (..)

import Http
import Json
import Maybe (Maybe (..), isJust)
import Maybe

parseTitle : String -> Maybe [Title]
parseTitle string =
    let toTitles json = case json of
            Json.Array vs -> map fromJust << filter isJust << map toTitle <| vs
            _ -> []
        toTitle json = case json of
            Json.String s -> Just (Title s)
            _ -> Nothing
    in (Maybe.map toTitles) << Json.fromString <| string

fromJust j = case j of
    Just v -> v

getTitles : Signal [Title]
getTitles = lift (Maybe.maybe [] identity << joinMaybe << Maybe.map parseTitle << extractString) <| Http.sendGet <| constant "http://localhost:3000/articles"

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
