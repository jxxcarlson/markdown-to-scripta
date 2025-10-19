module LaTeX.Renderer exposing (render)

import LaTeX.AST exposing (..)


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
        Section level title content ->
            let
                heading =
                    String.repeat level "#" ++ " " ++ title

                renderedContent =
                    if List.isEmpty content then
                        ""

                    else
                        "\n\n" ++ (content |> List.map renderBlock |> String.join "\n\n")
            in
            heading ++ renderedContent

        Paragraph inlines ->
            renderInlines inlines

        List listType items ->
            renderListItems listType items

        VerbatimBlock envName content ->
            case envName of
                "verbatim" ->
                    "| code\n" ++ indentLines content

                "code" ->
                    "| code\n" ++ indentLines content

                "equation" ->
                    "| equation\n" ++ content

                _ ->
                    "| " ++ envName ++ "\n" ++ content

        OrdinaryBlock envName blocks ->
            case envName of
                "theorem" ->
                    "| theorem\n" ++ (blocks |> List.map renderBlock |> String.join "\n")

                "lemma" ->
                    "| lemma\n" ++ (blocks |> List.map renderBlock |> String.join "\n")

                "proof" ->
                    "| proof\n" ++ (blocks |> List.map renderBlock |> String.join "\n")

                "quote" ->
                    "| quotation\n" ++ (blocks |> List.map renderBlock |> String.join "\n")

                _ ->
                    "| " ++ envName ++ "\n" ++ (blocks |> List.map renderBlock |> String.join "\n")

        BlankLine ->
            ""


{-| Indent each line by two spaces
-}
indentLines : String -> String
indentLines str =
    str
        |> String.lines
        |> List.map (\line -> "  " ++ line)
        |> String.join "\n"


{-| Render list items
-}
renderListItems : ListType -> List ListItem -> String
renderListItems listType items =
    items
        |> List.indexedMap (renderListItem listType)
        |> String.join "\n"


renderListItem : ListType -> Int -> ListItem -> String
renderListItem listType index inlines =
    let
        marker =
            case listType of
                Itemize ->
                    "- "

                Enumerate ->
                    String.fromInt (index + 1) ++ ". "
    in
    marker ++ renderInlines inlines


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
        Text text ->
            text

        Fun name args ->
            case name of
                "textbf" ->
                    "[b " ++ renderInlines args ++ "]"

                "textit" ->
                    "[i " ++ renderInlines args ++ "]"

                "emph" ->
                    "[i " ++ renderInlines args ++ "]"

                "texttt" ->
                    "`" ++ renderInlines args ++ "`"

                "code" ->
                    "`" ++ renderInlines args ++ "`"

                "href" ->
                    -- \href{url}{text} - but we only have args as combined
                    "[link " ++ renderInlines args ++ "]"

                -- Line break
                "\\" ->
                    "\n"

                -- Unknown command: just render the content
                _ ->
                    renderInlines args

        VFun name content ->
            case name of
                "math" ->
                    content

                "verb" ->
                    "`" ++ content ++ "`"

                "code" ->
                    "`" ++ content ++ "`"

                _ ->
                    content
