module MarkdownToScripta exposing (convert)

{-| Convert Markdown to Scripta markup language.

# Conversion
@docs convert

-}

import Markdown.AST exposing (Document)
import Markdown.Parser as Parser
import Markdown.Renderer as Renderer


{-| Convert Markdown source text to Scripta markup
-}
convert : String -> String
convert markdown =
    case Parser.parse markdown of
        Ok ast ->
            Renderer.render ast

        Err _ ->
            -- On parse error, return original text
            markdown