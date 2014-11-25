module Types where

import Graphics.Input as I

data CommentBlock = CommentBlock { collapsed : Bool, comments : [Comment], input : I.Input Bool }
data Paragraph = Paragraph String CommentBlock
data Comment = Comment Author String
data Article = Article Author Title [Paragraph]
data Title = Title String
data Author = Author String

data Screen = TitleScreen | ArticleScreen

screenInput : I.Input Screen
screenInput = I.input TitleScreen
