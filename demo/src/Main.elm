port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import LaTeXToScripta
import MarkdownToScripta


-- PORTS


port saveToLocalStorage : E.Value -> Cmd msg



-- MODEL


type InputMode
    = Markdown
    | LaTeX


type alias Model =
    { input : String
    , scripta : String
    , sources : List ( InputMode, String )
    , mode : InputMode
    }


type alias Flags =
    { markdownSource : Maybe String
    , latexSource : Maybe String
    , mode : Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        mode =
            case flags.mode of
                Just "LaTeX" ->
                    LaTeX

                _ ->
                    Markdown

        markdownSource =
            Maybe.withDefault exampleMarkdown flags.markdownSource

        latexSource =
            Maybe.withDefault exampleLaTeX flags.latexSource

        sources =
            [ ( Markdown, markdownSource )
            , ( LaTeX, latexSource )
            ]

        input =
            case mode of
                Markdown ->
                    markdownSource

                LaTeX ->
                    latexSource
    in
    ( { input = input
      , scripta = ""
      , sources = sources
      , mode = mode
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
            let
                -- Update the appropriate source in the sources list
                updatedSources =
                    List.map
                        (\( mode, source ) ->
                            if mode == model.mode then
                                ( mode, newInput )

                            else
                                ( mode, source )
                        )
                        model.sources

                newModel =
                    { model | input = newInput, sources = updatedSources }
            in
            ( newModel, saveToLocalStorage (encodeModel newModel) )

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
                -- Save current input to sources before switching
                updatedSources =
                    List.map
                        (\( mode, source ) ->
                            if mode == model.mode then
                                ( mode, model.input )

                            else
                                ( mode, source )
                        )
                        model.sources

                -- Get the source for the new mode
                newInput =
                    updatedSources
                        |> List.filter (\( mode, _ ) -> mode == newMode)
                        |> List.head
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault
                            (case newMode of
                                Markdown ->
                                    exampleMarkdown

                                LaTeX ->
                                    exampleLaTeX
                            )

                newModel =
                    { model | mode = newMode, input = newInput, sources = updatedSources, scripta = "" }
            in
            ( newModel, saveToLocalStorage (encodeModel newModel) )


{-| Encode model state for localStorage
-}
encodeModel : Model -> E.Value
encodeModel model =
    let
        getSource mode =
            model.sources
                |> List.filter (\( m, _ ) -> m == mode)
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault ""

        modeString =
            case model.mode of
                Markdown ->
                    "Markdown"

                LaTeX ->
                    "LaTeX"
    in
    E.object
        [ ( "markdownSource", E.string (getSource Markdown) )
        , ( "latexSource", E.string (getSource LaTeX) )
        , ( "mode", E.string modeString )
        ]


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
        ]


-- MAIN


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init << decodeFlags
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


{-| Decode flags from JavaScript
-}
decodeFlags : E.Value -> Flags
decodeFlags value =
    let
        decoder =
            D.map3 Flags
                (D.field "markdownSource" (D.nullable D.string))
                (D.field "latexSource" (D.nullable D.string))
                (D.field "mode" (D.nullable D.string))
    in
    case D.decodeValue decoder value of
        Ok flags ->
            flags

        Err _ ->
            -- Return empty flags if decoding fails
            { markdownSource = Nothing
            , latexSource = Nothing
            , mode = Nothing
            }