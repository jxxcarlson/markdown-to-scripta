module LaTeX.Renderer exposing (render)

import Dict
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

        List listType props items ->
            renderListItems listType props items

        VerbatimBlock envName props content ->
            let
                propsStr =
                    renderProperties props

                blockName =
                    case envName of
                        "verbatim" ->
                            "code"

                        "code" ->
                            "code"

                        "equation" ->
                            "equation"

                        _ ->
                            envName

                contentStr =
                    case envName of
                        "verbatim" ->
                            indentLines content

                        "code" ->
                            indentLines content

                        _ ->
                            content
            in
            "| " ++ blockName ++ propsStr ++ "\n" ++ contentStr

        OrdinaryBlock envName props blocks ->
            let
                propsStr =
                    renderProperties props

                blockName =
                    case envName of
                        "quote" ->
                            "quotation"

                        _ ->
                            envName
            in
            "| " ++ blockName ++ propsStr ++ "\n" ++ (blocks |> List.map renderBlock |> String.join "\n")

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
renderListItems : ListType -> Properties -> List ListItem -> String
renderListItems listType props items =
    case listType of
        Description ->
            -- Description lists need special handling with properties
            let
                propsStr =
                    renderProperties props
            in
            "| description" ++ propsStr ++ "\n" ++ (items |> List.map renderDescriptionItem |> String.join "\n")

        _ ->
            -- Regular lists (itemize, enumerate)
            items
                |> List.indexedMap (renderListItem listType)
                |> String.join "\n"


renderListItem : ListType -> Int -> ListItem -> String
renderListItem listType index item =
    let
        marker =
            case listType of
                Itemize ->
                    "- "

                Enumerate ->
                    String.fromInt (index + 1) ++ ". "

                Description ->
                    -- This shouldn't be called for description lists
                    "- "
    in
    marker ++ renderInlines item.content


{-| Render a description list item with its label
-}
renderDescriptionItem : ListItem -> String
renderDescriptionItem item =
    case item.label of
        Just label ->
            "  " ++ renderInlines label ++ " :: " ++ renderInlines item.content

        Nothing ->
            "  :: " ++ renderInlines item.content


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

                "text" ->
                    -- \text{...} in math mode -> "..."
                    "\"" ++ renderInlines args ++ "\""

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
                    -- Replace \text{...} with "..." in math mode
                    replaceTextCommands content

                "verb" ->
                    "`" ++ content ++ "`"

                "code" ->
                    "`" ++ content ++ "`"

                _ ->
                    content


{-| Replace \text{...} with "..." in math expressions
-}
replaceTextCommands : String -> String
replaceTextCommands str =
    case findTextCommand str of
        Nothing ->
            str

        Just ( before, textContent, after ) ->
            before ++ "\"" ++ textContent ++ "\"" ++ replaceTextCommands after


{-| Find the first \text{...} command and split the string into before, content, and after
-}
findTextCommand : String -> Maybe ( String, String, String )
findTextCommand str =
    case String.indexes "\\text{" str of
        [] ->
            Nothing

        firstIndex :: _ ->
            let
                -- Start after \text{
                contentStart =
                    firstIndex + 6

                -- Find the matching closing brace
                afterText =
                    String.dropLeft contentStart str
            in
            case findClosingBrace afterText 0 of
                Nothing ->
                    Nothing

                Just closingPos ->
                    let
                        before =
                            String.left firstIndex str

                        textContent =
                            String.left closingPos afterText

                        after =
                            String.dropLeft (closingPos + 1) afterText
                    in
                    Just ( before, textContent, after )


{-| Find the position of the closing brace, accounting for nesting
-}
findClosingBrace : String -> Int -> Maybe Int
findClosingBrace str pos =
    case String.uncons (String.dropLeft pos str) of
        Nothing ->
            Nothing

        Just ( char, _ ) ->
            if char == '}' then
                Just pos

            else if char == '{' then
                -- Found nested brace, need to find its closing brace first
                case findClosingBrace str (pos + 1) of
                    Nothing ->
                        Nothing

                    Just nestedClose ->
                        -- Continue after nested closing brace
                        findClosingBrace str (nestedClose + 1)

            else
                findClosingBrace str (pos + 1)


{-| Render properties dict to string format: " key:value, key2:value2"
Returns empty string if no properties
-}
renderProperties : Properties -> String
renderProperties props =
    if Dict.isEmpty props then
        ""

    else
        " "
            ++ (props
                    |> Dict.toList
                    |> List.map renderProperty
                    |> String.join ", "
               )


{-| Render a single property key-value pair
-}
renderProperty : ( String, String ) -> String
renderProperty ( key, value ) =
    if value == "?" then
        -- Standalone property without value
        key

    else
        key ++ ":" ++ value
