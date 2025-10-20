module LaTeX.Parser exposing (parse, deadEndsToString)

import Dict
import LaTeX.AST exposing (..)
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-| Custom problem type for better error messages
-}
type Problem
    = ExpectingSymbol String
    | ExpectingEnvironmentEnd String
    | ExpectingBlockContent
    | ExpectingInlineContent
    | EmptyParagraph
    | UnexpectedEndCommand
    | Custom String


{-| Context for tracking where we are in the parse
-}
type alias Context =
    String


{-| Parser type alias
-}
type alias LaTeXParser a =
    Parser Context Problem a


{-| Convert parse errors to a readable string
-}
deadEndsToString : List (Parser.DeadEnd Context Problem) -> String
deadEndsToString deadEnds =
    deadEnds
        |> List.map deadEndToString
        |> String.join "\n\n"


deadEndToString : Parser.DeadEnd Context Problem -> String
deadEndToString deadEnd =
    let
        position =
            "Line " ++ String.fromInt deadEnd.row ++ ", Column " ++ String.fromInt deadEnd.col

        problemMsg =
            case deadEnd.problem of
                ExpectingSymbol s ->
                    "Expecting symbol: " ++ s

                ExpectingEnvironmentEnd envName ->
                    "Expecting \\end{" ++ envName ++ "}"

                ExpectingBlockContent ->
                    "Expecting block content"

                ExpectingInlineContent ->
                    "Expecting inline content"

                EmptyParagraph ->
                    "Empty paragraph"

                UnexpectedEndCommand ->
                    "Unexpected \\end command"

                Custom msg ->
                    msg

        contextMsg =
            if List.isEmpty deadEnd.contextStack then
                ""

            else
                "\nContext: " ++ String.join " > " (List.map .context (List.reverse deadEnd.contextStack))
    in
    position ++ ": " ++ problemMsg ++ contextMsg


{-| Parse LaTeX text into an AST
-}
parse : String -> Result (List (Parser.DeadEnd Context Problem)) Document
parse input =
    Parser.run documentParser input


{-| Main document parser
-}
documentParser : LaTeXParser Document
documentParser =
    Parser.succeed identity
        |. spaces
        |= Parser.loop [] documentHelper
        |. Parser.end (ExpectingSymbol "end of input")


documentHelper : List Block -> LaTeXParser (Parser.Step (List Block) (List Block))
documentHelper blocks =
    Parser.oneOf
        [ -- If at end of input, we're done
          Parser.end (ExpectingSymbol "end of input")
            |> Parser.map (\_ -> Parser.Done blocks)
        , Parser.succeed (\_ -> Parser.Loop blocks)
            |= blankLineParser
        , Parser.succeed (\block -> Parser.Loop (blocks ++ [ block ]))
            |= blockParser
        ]


{-| Parse any block element
-}
blockParser : LaTeXParser Block
blockParser =
    Parser.inContext "block"
        (Parser.oneOf
            [ sectionParser
            , environmentParser
            , paragraphParser
            ]
        )


{-| Parse sections: \\section{}, \\subsection{}, \\subsubsection{}
-}
sectionParser : LaTeXParser Block
sectionParser =
    Parser.oneOf
        [ Parser.succeed (\title -> Section 1 title [])
            |. symbol "\\section"
            |. spaces
            |= braceContent
            |. Parser.oneOf [ symbol "\n", Parser.end (ExpectingSymbol "end") ]
        , Parser.succeed (\title -> Section 2 title [])
            |. symbol "\\subsection"
            |. spaces
            |= braceContent
            |. Parser.oneOf [ symbol "\n", Parser.end (ExpectingSymbol "end") ]
        , Parser.succeed (\title -> Section 3 title [])
            |. symbol "\\subsubsection"
            |. spaces
            |= braceContent
            |. Parser.oneOf [ symbol "\n", Parser.end (ExpectingSymbol "end") ]
        ]


{-| Parse content within braces: {content}
-}
braceContent : LaTeXParser String
braceContent =
    Parser.succeed identity
        |. symbol "{"
        |= Parser.getChompedString (Parser.chompUntil (Parser.Token "}" (ExpectingSymbol "}")))
        |. symbol "}"


{-| Parse environments: \\begin{name}[properties]...\\end{name}
-}
environmentParser : LaTeXParser Block
environmentParser =
    Parser.succeed Tuple.pair
        |. symbol "\\begin"
        |. spaces
        |. symbol "{"
        |= Parser.getChompedString (Parser.chompUntil (Parser.Token "}" (ExpectingSymbol "}")))
        |. symbol "}"
        |= optionalPropertiesParser
        |. spaces
        |. Parser.oneOf [ symbol "\n", Parser.succeed () ]
        |> Parser.andThen
            (\( envName, props ) ->
                Parser.inContext ("environment: " ++ envName)
                    (case envName of
                        "itemize" ->
                            listContentParser Itemize envName props

                        "enumerate" ->
                            listContentParser Enumerate envName props

                        "description" ->
                            listContentParser Description envName props

                        "verbatim" ->
                            verbatimContentParser envName props

                        "equation" ->
                            verbatimContentParser envName props

                        "align" ->
                            verbatimContentParser envName props

                        "code" ->
                            verbatimContentParser envName props

                        "lstlisting" ->
                            verbatimContentParser envName props

                        "tikzpicture" ->
                            tikzpictureContentParser envName props

                        "table" ->
                            tableContentParser envName props

                        _ ->
                            ordinaryBlockParser envName props
                    )
            )


{-| Extract \label{...} commands from content and return cleaned content with labels as properties
-}
extractLabels : String -> ( String, Properties )
extractLabels content =
    case findLabel content of
        Nothing ->
            ( content, Dict.empty )

        Just ( before, labelValue, after ) ->
            let
                ( restContent, restProps ) =
                    extractLabels after

                cleanedContent =
                    before ++ restContent
            in
            ( cleanedContent, Dict.insert "label" labelValue restProps )


{-| Find the first \label{...} command in the string
-}
findLabel : String -> Maybe ( String, String, String )
findLabel str =
    case String.indexes "\\label{" str of
        [] ->
            Nothing

        firstIndex :: _ ->
            let
                contentStart =
                    firstIndex + 7

                afterLabel =
                    String.dropLeft contentStart str
            in
            case findClosingBrace afterLabel 0 of
                Nothing ->
                    Nothing

                Just closingPos ->
                    let
                        before =
                            String.left firstIndex str

                        labelValue =
                            String.left closingPos afterLabel

                        after =
                            String.dropLeft (closingPos + 1) afterLabel
                    in
                    Just ( before, labelValue, after )


{-| Find the position of the closing brace for \label{}, accounting for nesting
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
                case findClosingBrace str (pos + 1) of
                    Nothing ->
                        Nothing

                    Just nestedClose ->
                        findClosingBrace str (nestedClose + 1)

            else
                findClosingBrace str (pos + 1)


{-| Parse optional properties in square brackets: [key=value, key=value]
-}
optionalPropertiesParser : LaTeXParser Properties
optionalPropertiesParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. symbol "["
            |= Parser.getChompedString (Parser.chompUntil (Parser.Token "]" (ExpectingSymbol "]")))
            |. symbol "]"
            |> Parser.map parseProperties
        , Parser.succeed Dict.empty
        ]


{-| Parse properties string into a Dict
Format: "key=value, key2=value2, standalone"
-}
parseProperties : String -> Properties
parseProperties str =
    str
        |> String.split ","
        |> List.map String.trim
        |> List.filterMap parseProperty
        |> Dict.fromList


{-| Parse a single property: "key=value" or "standalone"
-}
parseProperty : String -> Maybe ( String, String )
parseProperty str =
    case String.split "=" str of
        [ key ] ->
            -- Standalone property like "blahblah"
            Just ( String.trim key, "?" )

        [ key, value ] ->
            -- Key-value pair
            Just ( String.trim key, String.trim value )

        key :: rest ->
            -- Handle cases like "label=\arabic*" which might have special chars
            Just ( String.trim key, String.trim (String.join "=" rest) )

        [] ->
            Nothing


{-| Parse list content (items)
-}
listContentParser : ListType -> Name -> Properties -> LaTeXParser Block
listContentParser listType envName props =
    Parser.loop [] (listItemHelper listType envName)
        |> Parser.map (List listType props)


listItemHelper : ListType -> Name -> List ListItem -> LaTeXParser (Parser.Step (List ListItem) (List ListItem))
listItemHelper listType envName items =
    Parser.oneOf
        [ Parser.succeed (\item -> Parser.Loop (item :: items))
            |. spaces
            |= itemParser listType
        , Parser.succeed ()
            |. spaces
            |. symbol "\\end"
            |. spaces
            |. symbol "{"
            |. token envName
            |. symbol "}"
            |. Parser.oneOf [ symbol "\n", Parser.end (ExpectingSymbol "end") ]
            |> Parser.map (\_ -> Parser.Done (List.reverse items))
        ]


{-| Parse a single \\item, with optional [label] for description lists
-}
itemParser : ListType -> LaTeXParser ListItem
itemParser listType =
    Parser.succeed Tuple.pair
        |. symbol "\\item"
        |= (case listType of
                Description ->
                    -- Try to parse optional [label]
                    Parser.oneOf
                        [ Parser.succeed Just
                            |. spaces
                            |. symbol "["
                            |= (Parser.getChompedString (Parser.chompUntil (Parser.Token "]" (ExpectingSymbol "]")))
                                    |> Parser.andThen parseInlinesFromString
                               )
                            |. symbol "]"
                        , Parser.succeed Nothing
                        ]

                _ ->
                    Parser.succeed Nothing
           )
        |. spaces
        |= (Parser.getChompedString (Parser.chompUntilEndOr "\n")
                |> Parser.andThen parseInlinesFromString
           )
        |. Parser.oneOf [ symbol "\n", Parser.end (ExpectingSymbol "end") ]
        |> Parser.map (\( label, content ) -> { label = label, content = content })


{-| Parse verbatim content (doesn't parse inner structure)
-}
verbatimContentParser : Name -> Properties -> LaTeXParser Block
verbatimContentParser envName props =
    Parser.succeed
        (\content ->
            let
                ( cleanedContent, labelProps ) =
                    extractLabels content

                mergedProps =
                    Dict.union labelProps props
            in
            VerbatimBlock envName mergedProps (String.trim cleanedContent)
        )
        |= Parser.getChompedString (Parser.chompUntil (Parser.Token ("\\end{" ++ envName ++ "}") (ExpectingEnvironmentEnd envName)))
        |. symbol "\\end"
        |. spaces
        |. symbol "{"
        |. token envName
        |. symbol "}"
        |. Parser.oneOf [ symbol "\n", Parser.end (ExpectingSymbol "end") ]


{-| Parse tikzpicture content, preserving begin/end tags
-}
tikzpictureContentParser : Name -> Properties -> LaTeXParser Block
tikzpictureContentParser envName props =
    Parser.succeed
        (\content ->
            let
                -- Add get:image property
                tikzProps =
                    Dict.insert "get" "image" props

                -- Reconstruct full tikzpicture with begin/end tags
                fullContent =
                    "\\begin{tikzpicture}\n" ++ String.trim content ++ "\n\\end{tikzpicture}"
            in
            VerbatimBlock "tikz" tikzProps fullContent
        )
        |= Parser.getChompedString (Parser.chompUntil (Parser.Token ("\\end{" ++ envName ++ "}") (ExpectingEnvironmentEnd envName)))
        |. symbol "\\end"
        |. spaces
        |. symbol "{"
        |. token envName
        |. symbol "}"
        |. Parser.oneOf [ symbol "\n", Parser.end (ExpectingSymbol "end") ]


{-| Parse table content with caption, label, and tabular data
-}
tableContentParser : Name -> Properties -> LaTeXParser Block
tableContentParser envName props =
    Parser.succeed
        (\content ->
            let
                -- Transform standalone properties to table-format
                tableFormatProps =
                    transformTableFormatProps props

                tableProps =
                    extractTableProperties content

                tabularContent =
                    extractTabularContent content

                mergedProps =
                    Dict.union tableProps tableFormatProps
            in
            VerbatimBlock "table" mergedProps tabularContent
        )
        |= Parser.getChompedString (Parser.chompUntil (Parser.Token ("\\end{" ++ envName ++ "}") (ExpectingEnvironmentEnd envName)))
        |. symbol "\\end"
        |. spaces
        |. symbol "{"
        |. token envName
        |. symbol "}"
        |. Parser.oneOf [ symbol "\n", Parser.end (ExpectingSymbol "end") ]


{-| Transform standalone properties (value "?") to table-format property
-}
transformTableFormatProps : Properties -> Properties
transformTableFormatProps props =
    let
        standaloneProps =
            props
                |> Dict.toList
                |> List.filter (\( _, value ) -> value == "?")
                |> List.map Tuple.first

        regularProps =
            props
                |> Dict.filter (\_ value -> value /= "?")
    in
    case standaloneProps of
        [] ->
            regularProps

        first :: _ ->
            Dict.insert "table-format" first regularProps


{-| Extract caption and label from table content as properties
-}
extractTableProperties : String -> Properties
extractTableProperties content =
    let
        ( _, labelProps ) =
            extractLabels content

        captionProps =
            case findCaption content of
                Nothing ->
                    Dict.empty

                Just captionText ->
                    Dict.singleton "caption" captionText
    in
    Dict.union captionProps labelProps


{-| Find caption text in table content
-}
findCaption : String -> Maybe String
findCaption str =
    case String.indexes "\\caption{" str of
        [] ->
            Nothing

        firstIndex :: _ ->
            let
                contentStart =
                    firstIndex + 9

                afterCaption =
                    String.dropLeft contentStart str
            in
            case findClosingBrace afterCaption 0 of
                Nothing ->
                    Nothing

                Just closingPos ->
                    Just (String.left closingPos afterCaption)


{-| Extract and clean tabular content from table environment
-}
extractTabularContent : String -> String
extractTabularContent content =
    case String.indexes "\\begin{tabular}" content of
        [] ->
            ""

        firstIndex :: _ ->
            let
                -- Find where the column spec ends and content begins
                afterBegin =
                    String.dropLeft (firstIndex + 15) content

                -- Look for the closing brace of the column spec {columns}
                contentStart =
                    case String.indexes "}" afterBegin of
                        [] ->
                            0

                        idx :: _ ->
                            idx + 1

                tabularBody =
                    String.dropLeft contentStart afterBegin
                        |> String.split "\\end{tabular}"
                        |> List.head
                        |> Maybe.withDefault ""
            in
            cleanTableRows tabularBody


{-| Clean table rows by removing rules and processing data rows
-}
cleanTableRows : String -> String
cleanTableRows content =
    content
        |> String.lines
        |> List.filterMap cleanTableRow
        |> String.join "\n"
        |> String.trim


{-| Clean a single table row, returning Nothing for rows to skip
-}
cleanTableRow : String -> Maybe String
cleanTableRow line =
    let
        trimmed =
            String.trim line
    in
    -- Skip empty lines, rules, and commands
    if String.isEmpty trimmed then
        Nothing
    else if String.startsWith "\\" trimmed then
        -- Skip LaTeX commands like \toprule, \midrule, \bottomrule, \centering
        Nothing
    else if String.contains "&" trimmed then
        -- Keep data rows with column separators
        -- Remove trailing \\ and curly braces from cells
        Just (cleanRowContent trimmed)
    else
        Nothing


{-| Clean row content by removing trailing \\ and braces from cells
-}
cleanRowContent : String -> String
cleanRowContent row =
    row
        |> String.split "\\\\"
        |> List.head
        |> Maybe.withDefault row
        |> String.trim


{-| Parse ordinary block content (recursively parse blocks inside)
-}
ordinaryBlockParser : Name -> Properties -> LaTeXParser Block
ordinaryBlockParser envName props =
    Parser.loop [] (ordinaryBlockHelper envName)
        |> Parser.map (OrdinaryBlock envName props)


ordinaryBlockHelper : Name -> List Block -> LaTeXParser (Parser.Step (List Block) (List Block))
ordinaryBlockHelper envName blocks =
    Parser.oneOf
        [ Parser.succeed ()
            |. spaces
            |. symbol "\\end"
            |. spaces
            |. symbol "{"
            |. token envName
            |. symbol "}"
            |. Parser.oneOf [ symbol "\n", Parser.end (ExpectingSymbol "end") ]
            |> Parser.map (\_ -> Parser.Done (List.reverse blocks))
        , Parser.succeed (\block -> Parser.Loop (block :: blocks))
            |. spaces
            |= blockParser
        ]


{-| Parse paragraphs
-}
paragraphParser : LaTeXParser Block
paragraphParser =
    Parser.inContext "paragraph"
        (Parser.getChompedString (Parser.chompUntilEndOr "\n")
            |> Parser.andThen
                (\firstLine ->
                    if String.isEmpty firstLine then
                        Parser.problem EmptyParagraph

                    else
                        Parser.succeed firstLine
                            |. Parser.oneOf [ symbol "\n", Parser.end (ExpectingSymbol "end") ]
                            |> Parser.andThen
                                (\first ->
                                    Parser.loop [ first ] paragraphHelper
                                        |> Parser.andThen
                                            (\lines ->
                                                parseInlinesFromString (String.join " " lines)
                                                    |> Parser.map Paragraph
                                            )
                                )
                )
        )


paragraphHelper : List String -> LaTeXParser (Parser.Step (List String) (List String))
paragraphHelper lines =
    Parser.oneOf
        [ -- If at end of input, we're done
          Parser.end (ExpectingSymbol "end")
            |> Parser.map (\_ -> Parser.Done (List.reverse lines))
        , -- Try to parse another line, but use backtrackable to peek at first char
          Parser.backtrackable
            (Parser.getChompedString (Parser.chompIf (\_ -> True) (ExpectingSymbol "char"))
                |> Parser.andThen
                    (\c ->
                        if c == "\\" then
                            -- Hit backslash - fail so we backtrack and move to fallback
                            Parser.problem (Custom "hit backslash")
                        else if c == "\n" then
                            -- Hit newline - empty line, done
                            Parser.succeed (Parser.Done (List.reverse lines))
                        else
                            -- Normal char - get the rest of the line
                            Parser.getChompedString (Parser.chompWhile (\ch -> ch /= '\n' && ch /= '\\'))
                                |> Parser.andThen
                                    (\rest ->
                                        let
                                            line = c ++ rest
                                        in
                                        Parser.succeed (Parser.Loop (line :: lines))
                                            |. Parser.oneOf [ symbol "\n", Parser.end (ExpectingSymbol "end") ]
                                    )
                    )
            )
        , -- Fallback: done (hit backslash or something else)
          Parser.succeed (Parser.Done (List.reverse lines))
        ]


{-| Parse blank lines
-}
blankLineParser : LaTeXParser Block
blankLineParser =
    Parser.succeed BlankLine
        |. Parser.chompWhile (\c -> c == ' ' || c == '\t')
        |. symbol "\n"


{-| Parse inline elements from a String
-}
parseInlinesFromString : String -> LaTeXParser (List Inline)
parseInlinesFromString input =
    case Parser.run inlinesParser input of
        Ok inlines ->
            Parser.succeed inlines

        Err _ ->
            Parser.succeed [ Text input ]


{-| Main inline parser
-}
inlinesParser : LaTeXParser (List Inline)
inlinesParser =
    Parser.loop [] inlinesHelper


inlinesHelper : List Inline -> LaTeXParser (Parser.Step (List Inline) (List Inline))
inlinesHelper inlines =
    Parser.oneOf
        [ Parser.succeed (\inline -> Parser.Loop (inline :: inlines))
            |= inlineParser
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse inlines))
        ]


{-| Parse a single inline element
-}
inlineParser : LaTeXParser Inline
inlineParser =
    Parser.lazy (\_ ->
        Parser.oneOf
            [ commandParser
            , mathInlineParser
            , textParser
            ]
    )


{-| Parse LaTeX commands like \\textbf{}, \\emph{}, etc.
-}
commandParser : LaTeXParser Inline
commandParser =
    Parser.succeed identity
        |. symbol "\\"
        |= Parser.getChompedString (Parser.chompWhile Char.isAlpha)
        |> Parser.andThen
            (\cmdName ->
                Parser.oneOf
                    [ -- Commands with brace content
                      Parser.succeed (\content -> Fun cmdName content)
                        |. spaces
                        |. symbol "{"
                        |= braceInlineContent
                        |. symbol "}"
                    , -- Commands without arguments (like \\)
                      Parser.succeed (Fun cmdName [])
                    ]
            )


{-| Parse inline content within braces (recursively)
-}
braceInlineContent : LaTeXParser (List Inline)
braceInlineContent =
    Parser.lazy (\_ ->
        Parser.getChompedString (chompBraceContent 0)
            |> Parser.andThen parseInlinesFromString
    )


{-| Chomp content inside braces, handling nesting
-}
chompBraceContent : Int -> LaTeXParser ()
chompBraceContent initialDepth =
    Parser.loop initialDepth chompBraceHelper


chompBraceHelper : Int -> LaTeXParser (Parser.Step Int ())
chompBraceHelper depth =
    Parser.oneOf
        [ Parser.succeed (Parser.Loop (depth + 1))
            |. symbol "{"
        , -- Look ahead for } without consuming it when depth is 0
          Parser.backtrackable (symbol "}")
            |> Parser.andThen
                (\_ ->
                    if depth > 0 then
                        Parser.succeed (Parser.Loop (depth - 1))

                    else
                        -- Don't consume the closing brace, let caller handle it
                        Parser.problem (Custom "Done chomping content")
                )
        , Parser.succeed (Parser.Loop depth)
            |. Parser.chompIf (\c -> c /= '{' && c /= '}') (ExpectingSymbol "character")
        , Parser.succeed (Parser.Done ())
        ]


{-| Parse inline math: $...$
-}
mathInlineParser : LaTeXParser Inline
mathInlineParser =
    Parser.succeed (\content -> VFun "math" ("$" ++ content ++ "$"))
        |. symbol "$"
        |= Parser.getChompedString (Parser.chompUntil (Parser.Token "$" (ExpectingSymbol "$")))
        |. symbol "$"


{-| Parse plain text
-}
textParser : LaTeXParser Inline
textParser =
    Parser.succeed Text
        |= Parser.getChompedString
            (Parser.chompIf (\c -> c /= '\\' && c /= '$' && c /= '{' && c /= '}') (ExpectingSymbol "text character")
                |. Parser.chompWhile (\c -> c /= '\\' && c /= '$' && c /= '{' && c /= '}')
            )


{-| Helper: parse a symbol
-}
symbol : String -> LaTeXParser ()
symbol str =
    Parser.symbol (Parser.Token str (ExpectingSymbol str))


{-| Helper: parse a token (runtime string)
-}
token : String -> LaTeXParser ()
token str =
    Parser.token (Parser.Token str (ExpectingSymbol str))


{-| Helper: parse spaces
-}
spaces : LaTeXParser ()
spaces =
    Parser.chompWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')
