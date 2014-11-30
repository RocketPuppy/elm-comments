module Types where

import Graphics.Input as I

import Comments

type CommentsActionHandle = I.Handle Comments.Action

{--
data CommentBlock = CommentBlock { collapsed : Bool, comments : [Comment], input : I.Input Bool }
data Paragraph = Paragraph String CommentBlock
data Comment = Comment Author String
data Article = Article Author Title [Paragraph]
data Title = Title String
data Author = Author String

data Route = TitleRoute | ArticleRoute

type Renderer = I.Handle Route -> Signal Html
type RoutePattern = (Route, Renderer)
--}
