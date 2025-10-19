module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import LaTeXToScripta
import MarkdownToScripta


-- MODEL


type InputMode
    = Markdown
    | LaTeX


type alias Model =
    { input : String
    , scripta : String
    , mode : InputMode
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = exampleMarkdown
      , scripta = ""
      , mode = Markdown
      }
    , Cmd.none
    )


exampleMarkdown : String
exampleMarkdown =
    """# Welcome to Markdown

This is a **bold** statement and this is *italic*.

## Code Example

Here is some `inline code` and a block:
```python
def hello():
    print("Hello, World!")
```

## Lists

- First item
- Second item
  - Nested item
  - Another nested

## Links and Images

Check out [Google](https://google.com)

![A beautiful image](https://example.com/image.jpg)

## Blockquote

> This is a quoted text
> spanning multiple lines

## Table

| Name | Age |
|------|-----|
| Alice | 30 |
| Bob | 25 |

---

That's all folks!"""


exampleLaTeX : String
exampleLaTeX =
    """\\section{Introduction}

This is a \\textbf{bold} statement and this is \\textit{italic}.

\\subsection{Mathematical Expressions}

The famous equation $E=mc^2$ demonstrates mass-energy equivalence.

\\begin{equation}
a^2 + b^2 = c^2
\\end{equation}

\\subsection{Lists}

\\begin{itemize}
\\item First item
\\item Second item
\\item Third item
\\end{itemize}

\\begin{enumerate}
\\item First numbered item
\\item Second numbered item
\\end{enumerate}

\\subsection{Code}

Here is some \\texttt{inline code} and a block:

\\begin{verbatim}
def hello():
    print("Hello, World!")
\\end{verbatim}

\\subsection{Theorem}

\\begin{theorem}
There are infinitely many prime numbers.
\\end{theorem}

That's all folks!"""


-- UPDATE


type Msg
    = InputChanged String
    | Convert
    | SwitchMode InputMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged newInput ->
            ( { model | input = newInput }, Cmd.none )

        Convert ->
            let
                converted =
                    case model.mode of
                        Markdown ->
                            MarkdownToScripta.convert model.input

                        LaTeX ->
                            LaTeXToScripta.convert model.input
            in
            ( { model | scripta = converted }
            , Cmd.none
            )

        SwitchMode newMode ->
            let
                newInput =
                    case newMode of
                        Markdown ->
                            exampleMarkdown

                        LaTeX ->
                            exampleLaTeX
            in
            ( { model | mode = newMode, input = newInput, scripta = "" }
            , Cmd.none
            )


-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "Markdown/LaTeX to Scripta Converter" ]
        , p [ class "subtitle" ] [ text "Convert Markdown or LaTeX syntax to Scripta markup language" ]
        , div [ class "mode-toggle" ]
            [ button
                [ onClick (SwitchMode Markdown)
                , class
                    (if model.mode == Markdown then
                        "mode-btn active"

                     else
                        "mode-btn"
                    )
                ]
                [ text "Markdown" ]
            , button
                [ onClick (SwitchMode LaTeX)
                , class
                    (if model.mode == LaTeX then
                        "mode-btn active"

                     else
                        "mode-btn"
                    )
                ]
                [ text "LaTeX" ]
            ]
        , div [ class "grid" ]
            [ div [ class "column" ]
                [ label []
                    [ text
                        (case model.mode of
                            Markdown ->
                                "Markdown Input"

                            LaTeX ->
                                "LaTeX Input"
                        )
                    ]
                , textarea
                    [ value model.input
                    , onInput InputChanged
                    , rows 20
                    , class "editor"
                    ]
                    []
                ]
            , div [ class "column" ]
                [ label [] [ text "Scripta Output" ]
                , textarea
                    [ value model.scripta
                    , readonly True
                    , rows 20
                    , class "editor output"
                    ]
                    []
                ]
            ]
        , button [ onClick Convert, class "convert-btn" ] [ text "Convert to Scripta" ]
        , viewRules model.mode
        ]


viewRules : InputMode -> Html Msg
viewRules mode =
    div [ class "rules" ]
        [ h2 [] [ text "Conversion Rules" ]
        , div [ class "rules-grid" ]
            (case mode of
                Markdown ->
                    [ ruleItem "Headings" "Same as Markdown (# ## ###)"
                    , ruleItem "Bold/Italic" "**bold** → [b bold], *italic* → [i italic]"
                    , ruleItem "Links" "[text](url) → [link text url]"
                    , ruleItem "Images" "![alt](url) → [link alt url]"
                    , ruleItem "Code Blocks" "``` → | code"
                    , ruleItem "Blockquotes" "> text → | quotation"
                    ]

                LaTeX ->
                    [ ruleItem "Sections" "\\section{} → #, \\subsection{} → ##"
                    , ruleItem "Bold/Italic" "\\textbf{} → [b], \\textit{} → [i]"
                    , ruleItem "Math" "$...$ → $...$, \\begin{equation} → | equation"
                    , ruleItem "Code" "\\texttt{} → `, \\begin{verbatim} → | code"
                    , ruleItem "Lists" "\\begin{itemize} → -, \\begin{enumerate} → 1."
                    , ruleItem "Environments" "\\begin{theorem} → | theorem"
                    ]
            )
        ]


ruleItem : String -> String -> Html Msg
ruleItem title description =
    div [ class "rule-item" ]
        [ div [ class "rule-title" ] [ text title ]
        , div [ class "rule-desc" ] [ text description ]
        ]


-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }