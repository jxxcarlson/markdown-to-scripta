module LaTeX.AST exposing (Block(..), Inline(..), Document, ListItem, ListType(..), Name, Properties)

{-| Abstract Syntax Tree for LaTeX documents
-}

import Dict exposing (Dict)


type alias Name =
    String


type alias Properties =
    Dict String String


type alias Document =
    List Block


type Block
    = Section Int Name (List Block) -- level (1=section, 2=subsection, 3=subsubsection), title, content
    | Paragraph (List Inline)
    | List ListType Properties (List ListItem) -- list type, properties, items
    | VerbatimBlock Name Properties String -- environment name, properties, content
    | OrdinaryBlock Name Properties (List Block) -- environment name, properties, content blocks
    | BlankLine


type ListType
    = Itemize
    | Enumerate
    | Description


type alias ListItem =
    { label : Maybe (List Inline) -- optional label for description lists
    , content : List Inline
    }


type Inline
    = Text String
    | Fun Name (List Inline) -- function name, arguments (e.g., \emph{text})
    | VFun Name String -- verbatim function, content not parsed (e.g., \verb|text|)
