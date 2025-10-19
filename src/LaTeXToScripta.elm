module LaTeXToScripta exposing (convert)

{-| Convert LaTeX to Scripta markup language.

# Conversion
@docs convert

-}

import LaTeX.AST exposing (Document)
import LaTeX.Parser as Parser
import LaTeX.Renderer as Renderer


{-| Convert LaTeX source text to Scripta markup
-}
convert : String -> String
convert latex =
    case Parser.parse latex of
        Ok ast ->
            Renderer.render ast

        Err _ ->
            -- On parse error, return original text
            latex
