module Utils where

import String
import Http
import Json

joinMaybe : Maybe (Maybe a) -> Maybe a
joinMaybe m = case m of
    Just m' -> m'
    _ -> Nothing

subSpaces t = String.map toPlus t

fromJust j = case j of
    Just v -> v

extractString r = case r of
    Http.Success v -> Just v
    _ -> Nothing

isSuccess r = case r of
    Http.Success _ -> True
    _ -> False

jsonString json = case json of
    Json.String s -> s

toPlus c = if c == ' ' then '+' else c
