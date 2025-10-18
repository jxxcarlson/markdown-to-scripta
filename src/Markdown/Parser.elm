module Markdown.Parser exposing (parse)

import Markdown.AST exposing (..)
import Parser exposing (..)


{-| Parse Markdown text into an AST
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
        [ headingParser
        , horizontalRuleParser
        , mathBlockParser
        , fencedCodeBlockParser
        , indentedCodeBlockParser
        , blockquoteParser
        , tableParser
        , listParser
        , paragraphParser
        ]


{-| Parse headings (# Header)
-}
headingParser : Parser Block
headingParser =
    succeed (\level content -> Heading level content)
        |= (succeed String.length
                |= getChompedString (chompWhile (\c -> c == '#'))
                |> andThen
                    (\len ->
                        if len >= 1 && len <= 6 then
                            succeed len
                        else
                            problem "Invalid heading level"
                    )
           )
        |. symbol " "
        |= (getChompedString (chompUntilEndOr "\n")
                |> andThen (\str -> parseInlines str)
           )
        |. oneOf [ symbol "\n", end ]


{-| Parse horizontal rule (---, ***, ___)
-}
horizontalRuleParser : Parser Block
horizontalRuleParser =
    succeed HorizontalRule
        |. oneOf
            [ symbol "---"
            , symbol "***"
            , symbol "___"
            ]
        |. chompWhile (\c -> c == '-' || c == '*' || c == '_')
        |. oneOf [ symbol "\n", end ]


{-| Parse display math blocks ($$...$$)
-}
mathBlockParser : Parser Block
mathBlockParser =
    succeed MathBlock
        |. symbol "$$"
        |. oneOf [ symbol "\n", succeed () ]
        |= (getChompedString (chompUntil "$$")
                |> map String.trim
           )
        |. symbol "$$"
        |. oneOf [ symbol "\n", end ]


{-| Parse fenced code blocks (```)
-}
fencedCodeBlockParser : Parser Block
fencedCodeBlockParser =
    succeed (\lang code -> CodeBlock lang code)
        |. symbol "```"
        |= (getChompedString (chompUntilEndOr "\n")
                |> map
                    (\lang ->
                        if String.isEmpty (String.trim lang) then
                            Nothing
                        else
                            Just (String.trim lang)
                    )
           )
        |. oneOf [ symbol "\n", succeed () ]
        |= (getChompedString (chompUntil "```")
                |> map String.trimRight
           )
        |. symbol "```"
        |. oneOf [ symbol "\n", end ]


{-| Parse indented code blocks (4 spaces)
-}
indentedCodeBlockParser : Parser Block
indentedCodeBlockParser =
    succeed (\first rest -> CodeBlock Nothing (String.join "\n" (first :: rest)))
        |. symbol "    "
        |= getChompedString (chompUntilEndOr "\n")
        |. oneOf [ symbol "\n", succeed () ]
        |= loop [] indentedCodeHelper


indentedCodeHelper : List String -> Parser (Step (List String) (List String))
indentedCodeHelper lines =
    oneOf
        [ succeed (\line -> Loop (line :: lines))
            |. symbol "    "
            |= getChompedString (chompUntilEndOr "\n")
            |. oneOf [ symbol "\n", succeed () ]
        , succeed ()
            |> map (\_ -> Done (List.reverse lines))
        ]


{-| Parse blockquotes (> text)
-}
blockquoteParser : Parser Block
blockquoteParser =
    succeed identity
        |. symbol "> "
        |= getChompedString (chompUntilEndOr "\n")
        |. oneOf [ symbol "\n", succeed () ]
        |> andThen
            (\firstLine ->
                loop [ firstLine ] blockquoteHelper
                    |> andThen
                        (\lines ->
                            case run documentParser (String.join "\n" lines) of
                                Ok blocks ->
                                    succeed (Blockquote blocks)

                                Err _ ->
                                    succeed (Blockquote [ Paragraph [ Plain (String.join "\n" lines) ] ])
                        )
            )


blockquoteHelper : List String -> Parser (Step (List String) (List String))
blockquoteHelper lines =
    oneOf
        [ succeed (\line -> Loop (line :: lines))
            |. symbol "> "
            |= getChompedString (chompUntilEndOr "\n")
            |. oneOf [ symbol "\n", succeed () ]
        , succeed ()
            |> map (\_ -> Done (List.reverse lines))
        ]


{-| Parse tables
-}
tableParser : Parser Block
tableParser =
    succeed Table
        |= (succeed identity
                |= tableRowParser
                |> andThen
                    (\firstRow ->
                        succeed (\_ rest -> firstRow :: rest)
                            |= tableSeparatorParser
                            |= loop [] tableRowsHelper
                    )
           )


tableRowParser : Parser (List String)
tableRowParser =
    succeed (\row ->
        row
            |> String.split "|"
            |> List.map String.trim
            |> List.filter (not << String.isEmpty)
        )
        |. symbol "|"
        |= getChompedString (chompUntilEndOr "\n")
        |. oneOf [ symbol "\n", end ]


tableSeparatorParser : Parser ()
tableSeparatorParser =
    succeed ()
        |. symbol "|"
        |. chompWhile (\c -> c == '-' || c == ':' || c == ' ' || c == '|')
        |. oneOf [ symbol "\n", end ]


tableRowsHelper : List (List String) -> Parser (Step (List (List String)) (List (List String)))
tableRowsHelper rows =
    oneOf
        [ succeed (\row -> Loop (row :: rows))
            |= tableRowParser
        , succeed (Done (List.reverse rows))
        ]


{-| Parse lists (both ordered and unordered)
-}
listParser : Parser Block
listParser =
    listItemParser
        |> andThen
            (\firstItem ->
                loop [ firstItem ] listHelper
                    |> map List
            )


listHelper : List ListItem -> Parser (Step (List ListItem) (List ListItem))
listHelper items =
    oneOf
        [ succeed (\item -> Loop (item :: items))
            |= listItemParser
        , succeed ()
            |> map (\_ -> Done (List.reverse items))
        ]


listItemParser : Parser ListItem
listItemParser =
    oneOf
        [ unorderedListItemParser
        , orderedListItemParser
        ]


unorderedListItemParser : Parser ListItem
unorderedListItemParser =
    succeed (\indent content -> { indent = indent, ordered = False, number = Nothing, content = content })
        |= (getChompedString (chompWhile (\c -> c == ' '))
                |> map String.length
           )
        |. oneOf [ symbol "- ", symbol "* ", symbol "+ " ]
        |= (getChompedString (chompUntilEndOr "\n")
                |> andThen parseInlines
           )
        |. oneOf [ symbol "\n", end ]


orderedListItemParser : Parser ListItem
orderedListItemParser =
    succeed (\indent num content -> { indent = indent, ordered = True, number = Just num, content = content })
        |= (getChompedString (chompWhile (\c -> c == ' '))
                |> map String.length
           )
        |= int
        |. symbol ". "
        |= (getChompedString (chompUntilEndOr "\n")
                |> andThen parseInlines
           )
        |. oneOf [ symbol "\n", end ]


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
                        |> andThen (\first ->
                            loop [ first ] paragraphHelper
                                |> andThen (\lines ->
                                    parseInlines (String.join " " lines)
                                        |> map Paragraph
                                )
                        )
            )


paragraphHelper : List String -> Parser (Step (List String) (List String))
paragraphHelper lines =
    oneOf
        [ getChompedString (chompUntilEndOr "\n")
            |> andThen (\line ->
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


{-| Parse inline elements
-}
parseInlines : String -> Parser (List Inline)
parseInlines input =
    case run inlinesParser input of
        Ok inlines ->
            succeed inlines

        Err _ ->
            succeed [ Plain input ]


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


inlineParser : Parser Inline
inlineParser =
    oneOf
        [ imageParser
        , linkParser
        , boldParser
        , italicParser
        , mathParser
        , codeParser
        , plainParser
        ]


{-| Parse images ![alt](url)
-}
imageParser : Parser Inline
imageParser =
    succeed Image
        |. symbol "!["
        |= getChompedString (chompUntilEndOr "]")
        |. symbol "]("
        |= getChompedString (chompUntilEndOr ")")
        |. symbol ")"


{-| Parse links [text](url)
-}
linkParser : Parser Inline
linkParser =
    succeed Link
        |. symbol "["
        |= getChompedString (chompUntilEndOr "]")
        |. symbol "]("
        |= getChompedString (chompUntilEndOr ")")
        |. symbol ")"


{-| Parse bold **text** or __text__
-}
boldParser : Parser Inline
boldParser =
    oneOf
        [ succeed Bold
            |. symbol "**"
            |= (getChompedString (chompUntilEndOr "**")
                    |> andThen parseInlines
               )
            |. symbol "**"
        , succeed Bold
            |. symbol "__"
            |= (getChompedString (chompUntilEndOr "__")
                    |> andThen parseInlines
               )
            |. symbol "__"
        ]


{-| Parse italic *text* or _text_
-}
italicParser : Parser Inline
italicParser =
    oneOf
        [ succeed Italic
            |. symbol "*"
            |= (getChompedString (chompUntil "*")
                    |> andThen parseInlines
               )
            |. symbol "*"
        , succeed Italic
            |. symbol "_"
            |= (getChompedString (chompUntil "_")
                    |> andThen parseInlines
               )
            |. symbol "_"
        ]


{-| Parse inline math $math$ (no whitespace after opening or before closing $)
-}
mathParser : Parser Inline
mathParser =
    succeed Math
        |. symbol "$"
        |= (getChompedString
                (succeed ()
                    |. chompIf (\c -> c /= ' ' && c /= '\t' && c /= '\n')
                    |. chompUntil "$"
                )
                |> andThen (\str ->
                    if String.endsWith " " str || String.endsWith "\t" str || String.endsWith "\n" str then
                        problem "Math cannot end with whitespace"
                    else
                        succeed ("$" ++ str ++ "$")
                ))
        |. symbol "$"


{-| Parse inline code `code`
-}
codeParser : Parser Inline
codeParser =
    succeed Code
        |. symbol "`"
        |= getChompedString (chompUntilEndOr "`")
        |. symbol "`"


{-| Parse plain text
-}
plainParser : Parser Inline
plainParser =
    succeed Plain
        |= (getChompedString
                (chompIf
                    (\c -> c /= '*' && c /= '_' && c /= '`' && c /= '$' && c /= '[' && c /= '!' && c /= '\n')
                    |. chompWhile (\c -> c /= '*' && c /= '_' && c /= '`' && c /= '$' && c /= '[' && c /= '!' && c /= '\n')
                )
           )