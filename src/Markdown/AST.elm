module Markdown.AST exposing (Block(..), Inline(..), Document, ListItem)

{-| Abstract Syntax Tree for Markdown/Scripta documents
-}

type alias Document =
    List Block


type Block
    = Heading Int (List Inline)
    | Paragraph (List Inline)
    | CodeBlock (Maybe String) String
    | MathBlock String
    | List (List ListItem)
    | Blockquote (List Block)
    | Table (List (List String))
    | HorizontalRule
    | BlankLine


type alias ListItem =
    { indent : Int
    , ordered : Bool
    , number : Maybe Int
    , content : List Inline
    }


type Inline
    = Plain String
    | Bold (List Inline)
    | Italic (List Inline)
    | Code String
    | Math String
    | Link String String  -- text, url
    | Image String String  -- alt/caption, url