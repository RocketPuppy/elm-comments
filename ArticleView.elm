module ArticleView  where

import Types (..)

import Graphics.Input as I
import Maybe

import Html
import Html (text, Html)
import Html.Tags (..)
import Html.Attributes as A
import Html.Events (..)

paragraphText : String
paragraphText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas non tortor vel leo porta convallis. Sed dapibus lacus sit amet venenatis lobortis. Maecenas sed tempor tortor. Aliquam iaculis risus dui, blandit molestie nisl aliquam quis. Curabitur porttitor finibus sodales. Donec pharetra, ante vitae rhoncus laoreet, velit nibh rutrum lorem, et accumsan diam massa ut nisi. Nunc luctus euismod orci eu sollicitudin. Vivamus blandit dictum risus, suscipit euismod lectus iaculis et. Proin aliquam ligula sagittis, hendrerit est et, feugiat justo. Quisque laoreet nec urna at volutpat."

theArticle =
    (Article
        (Author "Daniel Wilson")
        (Title "Article Title")
        [ Paragraph
            paragraphText
            (CommentBlock
                { collapsed = True
                , comments =
                    [ Comment
                        (Author "Sally Seashore")
                        "I like this first paragraph"
                    ]
                , input = I.input True
                })
        , Paragraph
            paragraphText
            (CommentBlock
                { collapsed = True
                , comments =
                    [ Comment
                        (Author "Sally Seashore")
                        "This was a cool idea"
                    , Comment
                        (Author "Sally Seashore")
                        "I retract my previous statement"
                    ]
                , input = I.input True
                })
        ]
    )

mkArticle : Article -> Html
mkArticle a = case a of
    Article author title ps -> article [] <| [mkTitle title] ++ map mkParagraph ps ++ [mkAuthor author]

mkTitle : Title -> Html
mkTitle t = case t of
    Title title -> header [] [text title]

mkAuthor : Author -> Html
mkAuthor a = case a of
    Author author -> footer [] [text author]

mkParagraph : Paragraph -> Html
mkParagraph para = case para of
    Paragraph para comments -> div [A.class "paragraph"] <| (p [] [text para])::[mkCommentBlock comments]

mkCommentBlock : CommentBlock -> Html
mkCommentBlock comments = case comments of
    CommentBlock comments ->
        let className = if comments.collapsed then "comment-block collapsed" else "comment-block"
        in
        div [A.class className, onclick comments.input.handle (\_ -> not comments.collapsed)] <| (span [A.class "button"] [text "+"])::map mkComment comments.comments

mkComment : Comment -> Html
mkComment c = case c of
    Comment author comment -> div [A.class "comment"] <| (p [] [text comment])::[mkAuthor author]

-- handles the collapsing of comment blocks by creating a signal of paragraphs that emits paragraphs with either collapsed or uncollapsed comment boxes
paragraphs2Signal: Paragraph -> Signal Paragraph
paragraphs2Signal p = case p of
    Paragraph para (CommentBlock commentblock) -> (\collapsed -> Paragraph para (CommentBlock { commentblock | collapsed <- collapsed })) <~ commentblock.input.signal

-- converts an article to a signal by wrapping a signal of paragraphs
article2Signal : Article -> Signal Article
article2Signal a = case a of
    Article author title ps -> (Article author title) <~ (combine (map paragraphs2Signal ps))

render : Signal (Maybe Article) -> Renderer
render a _ = (Maybe.maybe (text "failed") (\a -> mkArticle a)) <~ a
-- should be:
-- render a _ = (Maybe.maybe (constant <| text "failed") (\a -> mkArticle <~ article2Signal a) <~ a

--testRender = render (Maybe.Just <~ article2Signal theArticle)
