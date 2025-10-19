module LaTeX.AST exposing (Block(..), Inline(..), Document, ListItem, ListType(..), Name)

{-| Abstract Syntax Tree for LaTeX documents
-}


type alias Name =
    String


type alias Document =
    List Block


type Block
    = Section Int Name (List Block) -- level (1=section, 2=subsection, 3=subsubsection), title, content
    | Paragraph (List Inline)
    | List ListType (List ListItem)
    | VerbatimBlock Name String -- environment name, content
    | OrdinaryBlock Name (List Block) -- environment name, content blocks
    | BlankLine


type ListType
    = Itemize
    | Enumerate


type alias ListItem =
    List Inline


type Inline
    = Text String
    | Fun Name (List Inline) -- function name, arguments (e.g., \emph{text})
    | VFun Name String -- verbatim function, content not parsed (e.g., \verb|text|)
