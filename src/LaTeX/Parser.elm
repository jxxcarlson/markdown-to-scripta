module LaTeX.Parser exposing (parse)

import LaTeX.AST exposing (..)
import Parser exposing (..)


{-| Parse LaTeX text into an AST
-}
parse : String -> Result (List DeadEnd) Document
parse input =
    run documentParser input


{-| Main document parser
-}
documentParser : Parser Document
documentParser =
    succeed identity
        |. spaces
        |= loop [] documentHelper
        |. end


documentHelper : List Block -> Parser (Step (List Block) (List Block))
documentHelper blocks =
    oneOf
        [ succeed (\_ -> Loop blocks)
            |= blankLineParser
        , succeed (\block -> Loop (blocks ++ [ block ]))
            |= blockParser
        , succeed ()
            |> map (\_ -> Done blocks)
        ]


{-| Parse any block element
-}
blockParser : Parser Block
blockParser =
    oneOf
        [ sectionParser
        , environmentParser
        , paragraphParser
        ]


{-| Parse sections: \section{}, \subsection{}, \subsubsection{}
-}
sectionParser : Parser Block
sectionParser =
    oneOf
        [ succeed (\title -> Section 1 title [])
            |. symbol "\\section"
            |. spaces
            |= braceContent
            |. oneOf [ symbol "\n", end ]
        , succeed (\title -> Section 2 title [])
            |. symbol "\\subsection"
            |. spaces
            |= braceContent
            |. oneOf [ symbol "\n", end ]
        , succeed (\title -> Section 3 title [])
            |. symbol "\\subsubsection"
            |. spaces
            |= braceContent
            |. oneOf [ symbol "\n", end ]
        ]


{-| Parse content within braces: {content}
-}
braceContent : Parser String
braceContent =
    succeed identity
        |. symbol "{"
        |= getChompedString (chompUntil "}")
        |. symbol "}"


{-| Parse environments: \begin{name}...\end{name}
-}
environmentParser : Parser Block
environmentParser =
    succeed identity
        |. symbol "\\begin"
        |. spaces
        |. symbol "{"
        |= getChompedString (chompUntil "}")
        |. symbol "}"
        |. spaces
        |. oneOf [ symbol "\n", succeed () ]
        |> andThen
            (\envName ->
                case envName of
                    "itemize" ->
                        listContentParser Itemize envName

                    "enumerate" ->
                        listContentParser Enumerate envName

                    "verbatim" ->
                        verbatimContentParser envName

                    "equation" ->
                        verbatimContentParser envName

                    "code" ->
                        verbatimContentParser envName

                    _ ->
                        ordinaryBlockParser envName
            )


{-| Parse list content (items)
-}
listContentParser : ListType -> Name -> Parser Block
listContentParser listType envName =
    loop [] (listItemHelper envName)
        |> map (List listType)


listItemHelper : Name -> List ListItem -> Parser (Step (List ListItem) (List ListItem))
listItemHelper envName items =
    oneOf
        [ succeed (\item -> Loop (item :: items))
            |. spaces
            |= itemParser
        , succeed ()
            |. spaces
            |. symbol "\\end"
            |. spaces
            |. symbol "{"
            |. token envName
            |. symbol "}"
            |. oneOf [ symbol "\n", end ]
            |> map (\_ -> Done (List.reverse items))
        ]


{-| Parse a single \item
-}
itemParser : Parser ListItem
itemParser =
    succeed identity
        |. symbol "\\item"
        |. spaces
        |= (getChompedString (chompUntilEndOr "\n")
                |> andThen parseInlinesFromString
           )
        |. oneOf [ symbol "\n", end ]


{-| Parse verbatim content (doesn't parse inner structure)
-}
verbatimContentParser : Name -> Parser Block
verbatimContentParser envName =
    succeed (\content -> VerbatimBlock envName (String.trim content))
        |= getChompedString (chompUntil ("\\end{" ++ envName ++ "}"))
        |. symbol "\\end"
        |. spaces
        |. symbol "{"
        |. token envName
        |. symbol "}"
        |. oneOf [ symbol "\n", end ]


{-| Parse ordinary block content (recursively parse blocks inside)
-}
ordinaryBlockParser : Name -> Parser Block
ordinaryBlockParser envName =
    loop [] (ordinaryBlockHelper envName)
        |> map (OrdinaryBlock envName)


ordinaryBlockHelper : Name -> List Block -> Parser (Step (List Block) (List Block))
ordinaryBlockHelper envName blocks =
    succeed identity
        |. spaces
        |> andThen
            (\_ ->
                oneOf
                    [ succeed ()
                        |. symbol "\\end"
                        |. spaces
                        |. symbol "{"
                        |. token envName
                        |. symbol "}"
                        |. oneOf [ symbol "\n", end ]
                        |> map (\_ -> Done (List.reverse blocks))
                    , succeed (\block -> Loop (block :: blocks))
                        |= blockParser
                    ]
            )


{-| Parse paragraphs
-}
paragraphParser : Parser Block
paragraphParser =
    getChompedString (chompUntilEndOr "\n")
        |> andThen
            (\firstLine ->
                if String.isEmpty firstLine then
                    problem "Empty paragraph"

                else
                    succeed firstLine
                        |. oneOf [ symbol "\n", end ]
                        |> andThen
                            (\first ->
                                loop [ first ] paragraphHelper
                                    |> andThen
                                        (\lines ->
                                            parseInlinesFromString (String.join " " lines)
                                                |> map Paragraph
                                        )
                            )
            )


paragraphHelper : List String -> Parser (Step (List String) (List String))
paragraphHelper lines =
    oneOf
        [ -- Check if next character is backslash (start of command/environment)
          backtrackable (symbol "\\")
            |> map (\_ -> Done (List.reverse lines))
        , getChompedString (chompUntilEndOr "\n")
            |> andThen
                (\line ->
                    if String.isEmpty line then
                        succeed (Done (List.reverse lines))

                    else
                        succeed (Loop (line :: lines))
                            |. oneOf [ symbol "\n", end ]
                )
        , succeed (Done (List.reverse lines))
        ]


{-| Parse blank lines
-}
blankLineParser : Parser Block
blankLineParser =
    succeed BlankLine
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |. symbol "\n"


{-| Helper to chomp until one of several strings
-}
chompUntilOneOf : List String -> Parser ()
chompUntilOneOf strings =
    chompWhile
        (\c ->
            not
                (List.any
                    (\s -> String.startsWith s (String.fromChar c))
                    strings
                )
        )


{-| Parse inline elements from a Parser that chomps content
-}
parseInlines : Parser () -> Parser (List Inline)
parseInlines contentParser =
    getChompedString contentParser
        |> andThen parseInlinesFromString


{-| Parse inline elements from a String
-}
parseInlinesFromString : String -> Parser (List Inline)
parseInlinesFromString input =
    case run inlinesParser input of
        Ok inlines ->
            succeed inlines

        Err _ ->
            succeed [ Text input ]


{-| Main inline parser
-}
inlinesParser : Parser (List Inline)
inlinesParser =
    loop [] inlinesHelper


inlinesHelper : List Inline -> Parser (Step (List Inline) (List Inline))
inlinesHelper inlines =
    oneOf
        [ succeed (\inline -> Loop (inline :: inlines))
            |= inlineParser
        , succeed ()
            |> map (\_ -> Done (List.reverse inlines))
        ]


{-| Parse a single inline element
-}
inlineParser : Parser Inline
inlineParser =
    lazy (\_ ->
        oneOf
            [ commandParser
            , mathInlineParser
            , textParser
            ]
    )


{-| Parse LaTeX commands like \textbf{}, \emph{}, etc.
-}
commandParser : Parser Inline
commandParser =
    succeed identity
        |. symbol "\\"
        |= getChompedString (chompWhile Char.isAlpha)
        |> andThen
            (\cmdName ->
                oneOf
                    [ -- Commands with brace content
                      succeed (\content -> Fun cmdName content)
                        |. spaces
                        |. symbol "{"
                        |= braceInlineContent
                        |. symbol "}"
                    , -- Commands without arguments (like \\)
                      succeed (Fun cmdName [])
                    ]
            )


{-| Parse inline content within braces (recursively)
-}
braceInlineContent : Parser (List Inline)
braceInlineContent =
    lazy (\_ ->
        getChompedString (chompBraceContent 0)
            |> andThen parseInlinesFromString
    )


{-| Chomp content inside braces, handling nesting
-}
chompBraceContent : Int -> Parser ()
chompBraceContent initialDepth =
    loop initialDepth chompBraceHelper


chompBraceHelper : Int -> Parser (Step Int ())
chompBraceHelper depth =
    oneOf
        [ succeed (Loop (depth + 1))
            |. symbol "{"
        , -- Look ahead for } without consuming it when depth is 0
          backtrackable (symbol "}")
            |> andThen
                (\_ ->
                    if depth > 0 then
                        succeed (Loop (depth - 1))

                    else
                        -- Don't consume the closing brace, let caller handle it
                        problem "Done chomping content"
                )
        , succeed (Loop depth)
            |. chompIf (\c -> c /= '{' && c /= '}')
        , succeed (Done ())
        ]


{-| Parse inline math: $...$
-}
mathInlineParser : Parser Inline
mathInlineParser =
    succeed (\content -> VFun "math" ("$" ++ content ++ "$"))
        |. symbol "$"
        |= getChompedString (chompUntil "$")
        |. symbol "$"


{-| Parse plain text
-}
textParser : Parser Inline
textParser =
    succeed Text
        |= getChompedString
            (chompIf (\c -> c /= '\\' && c /= '$' && c /= '{' && c /= '}')
                |. chompWhile (\c -> c /= '\\' && c /= '$' && c /= '{' && c /= '}')
            )
