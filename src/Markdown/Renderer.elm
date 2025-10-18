module Markdown.Renderer exposing (render)

import Markdown.AST exposing (..)


{-| Render AST to Scripta markup
-}
render : Document -> String
render blocks =
    blocks
        |> List.map renderBlock
        |> List.filter (not << String.isEmpty)
        |> String.join "\n\n"
        |> ensureTrailingNewline


ensureTrailingNewline : String -> String
ensureTrailingNewline str =
    if String.endsWith "\n" str then
        str
    else
        str ++ "\n"


{-| Render a single block
-}
renderBlock : Block -> String
renderBlock block =
    case block of
        Heading level inlines ->
            String.repeat level "#" ++ " " ++ renderInlines inlines

        Paragraph inlines ->
            renderInlines inlines

        CodeBlock maybeLang code ->
            let
                indentedCode =
                    code
                        |> String.lines
                        |> List.map (\line -> "  " ++ line)
                        |> String.join "\n"
            in
            "| code" ++ (maybeLang |> Maybe.map (\lang -> " " ++ lang) |> Maybe.withDefault "") ++ "\n" ++ indentedCode

        MathBlock math ->
            "| equation\n" ++ math

        List items ->
            renderListItems items

        Blockquote blocks ->
            "| quotation\n" ++ (blocks |> List.map renderBlock |> String.join "\n")


        Table rows ->
                    let
                      nColumns =
                        case (rows |> List.head) of
                            Just firstRow ->
                                List.length firstRow

                            Nothing ->
                                0

                      format = String.repeat nColumns "l"

                        --headerSeparator =
                        --    List.repeat nColumns "---" |> String.join " & " ++ " \\\\"
                     in
                    "| csvtable " ++ format++ "\n" ++ (rows |> List.map renderCSVTableRow |> String.join "\n")

        HorizontalRule ->
            "[hrule]"

        BlankLine ->
            ""


{-| Render table row
-}
renderTableRow : List String -> String
renderTableRow cells =
    String.join " & " cells ++ " \\\\"

renderCSVTableRow : List String -> String
renderCSVTableRow cells =
    String.join ", " cells


{-| Render list items
-}
renderListItems : List ListItem -> String
renderListItems items =
    items
        |> List.map renderListItem
        |> String.join "\n"


renderListItem : ListItem -> String
renderListItem item =
    let
        indent =
            String.repeat item.indent " "

        marker =
            if item.ordered then
                String.fromInt (item.number |> Maybe.withDefault 1) ++ ". "
            else
                "- "
    in
    indent ++ marker ++ renderInlines item.content


{-| Render inline elements
-}
renderInlines : List Inline -> String
renderInlines inlines =
    inlines
        |> List.map renderInline
        |> String.join ""


renderInline : Inline -> String
renderInline inline =
    case inline of
        Plain text ->
            text

        Bold inlines ->
            "[b " ++ renderInlines inlines ++ "]"

        Italic inlines ->
            "[i " ++ renderInlines inlines ++ "]"

        Code code ->
            "`" ++ code ++ "`"

        Math math ->
            math

        Link text url ->
            "[link " ++ text ++ " " ++ url ++ "]"

        Image alt url ->
            "[link " ++ (if String.isEmpty alt then "image" else alt) ++ " " ++ url ++ "]"