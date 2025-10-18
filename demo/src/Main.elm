module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MarkdownToScripta


-- MODEL


type alias Model =
    { markdown : String
    , scripta : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { markdown = exampleMarkdown
      , scripta = ""
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


-- UPDATE


type Msg
    = MarkdownChanged String
    | Convert


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MarkdownChanged newMarkdown ->
            ( { model | markdown = newMarkdown }, Cmd.none )

        Convert ->
            ( { model | scripta = MarkdownToScripta.convert model.markdown }
            , Cmd.none
            )


-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "Markdown to Scripta Converter" ]
        , p [ class "subtitle" ] [ text "Convert Markdown syntax to Scripta markup language" ]
        , div [ class "grid" ]
            [ div [ class "column" ]
                [ label [] [ text "Markdown Input" ]
                , textarea
                    [ value model.markdown
                    , onInput MarkdownChanged
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
        , viewRules
        ]


viewRules : Html Msg
viewRules =
    div [ class "rules" ]
        [ h2 [] [ text "Conversion Rules" ]
        , div [ class "rules-grid" ]
            [ ruleItem "Headings" "Same as Markdown (# ## ###)"
            , ruleItem "Bold/Italic" "**bold** → [b bold], *italic* → [i italic]"
            , ruleItem "Links" "[text](url) → [link text url]"
            , ruleItem "Images" "![alt](url) → [link alt url]"
            , ruleItem "Code Blocks" "``` → | code"
            , ruleItem "Blockquotes" "> text → | quotation"
            ]
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