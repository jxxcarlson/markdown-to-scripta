module LaTeXTests exposing (main)

import Html exposing (Html, div, h3, pre, text)
import Html.Attributes
import LaTeXToScripta
import Parser exposing (DeadEnd)


type alias Test =
    { name : String
    , latex : String
    , expected : String
    }


tests : List Test
tests =
    [ { name = "Plain text"
      , latex = "Hello, World!"
      , expected = "Hello, World!"
      }
    , { name = "Bold text"
      , latex = "This is \\textbf{bold} text."
      , expected = "This is [b bold] text."
      }
    , { name = "Italic text"
      , latex = "This is \\textit{italic} text."
      , expected = "This is [i italic] text."
      }
    , { name = "Emphasis"
      , latex = "This is \\emph{emphasized} text."
      , expected = "This is [i emphasized] text."
      }
    , { name = "Typewriter/code"
      , latex = "Use \\texttt{code} for code."
      , expected = "Use `code` for code."
      }
    , { name = "Inline math"
      , latex = "The equation $E=mc^2$ is famous."
      , expected = "The equation $E=mc^2$ is famous."
      }
    , { name = "Section"
      , latex = "\\section{Introduction}\n\nSome text here."
      , expected = "# Introduction\n\nSome text here."
      }
    , { name = "Subsection"
      , latex = "\\subsection{Background}\n\nMore text."
      , expected = "## Background\n\nMore text."
      }
    , { name = "Nested formatting"
      , latex = "This is \\textbf{bold and \\textit{italic}} text."
      , expected = "This is [b bold and [i italic]] text."
      }
    , { name = "Itemize list"
      , latex = "\\begin{itemize}\n\\item First item\n\\item Second item\n\\end{itemize}"
      , expected = "- First item\n- Second item"
      }
    , { name = "Enumerate list"
      , latex = "\\begin{enumerate}\n\\item First\n\\item Second\n\\item Third\n\\end{enumerate}"
      , expected = "1. First\n2. Second\n3. Third"
      }
    , { name = "Verbatim block"
      , latex = "\\begin{verbatim}\nfor i in range(5):\n    print(i)\n\\end{verbatim}"
      , expected = "| code\n  for i in range(5):\n      print(i)"
      }
    , { name = "Equation environment"
      , latex = "\\begin{equation}\na^2 + b^2 = c^2\n\\end{equation}"
      , expected = "| equation\na^2 + b^2 = c^2"
      }
    , { name = "Ordinary block (theorem)"
      , latex = "\\begin{theorem}\nThere are infinitely many primes.\n\\end{theorem}"
      , expected = "| theorem\nThere are infinitely many primes."
      }
    , { name = "Debug - minimal theorem"
      , latex = "\\begin{theorem}\ntest\n\\end{theorem}"
      , expected = "| theorem\ntest"
      }
    , { name = "Debug - empty theorem"
      , latex = "\\begin{theorem}\\end{theorem}"
      , expected = "| theorem"
      }
    , { name = "Debug theorem - just text"
      , latex = "There are infinitely many primes."
      , expected = "There are infinitely many primes."
      }
    , { name = "Multiple paragraphs"
      , latex = "First paragraph here.\n\nSecond paragraph here."
      , expected = "First paragraph here.\n\nSecond paragraph here."
      }
    , { name = "Combined formatting and math"
      , latex = "We have \\textbf{bold} and $x^2$ in one line."
      , expected = "We have [b bold] and $x^2$ in one line."
      }
    ]


testOne : Test -> Html msg
testOne test =
    let
        actual =
            LaTeXToScripta.convert test.latex
                |> String.trim

        expectedTrimmed =
            String.trim test.expected

        status =
            if actual == expectedTrimmed then
                "✓"

            else
                "✗"
    in
    div []
        [ h3 [] [ text (status ++ " " ++ test.name) ]
        , pre [] [ text ("Input LaTeX:\n" ++ test.latex) ]
        , pre [] [ text ("Expected:\n" ++ expectedTrimmed) ]
        , pre [] [ text ("Actual:\n" ++ actual) ]
        , if status == "✗" then
            pre [ Html.Attributes.style "color" "red" ]
                [ text ("MISMATCH!") ]

          else
            text ""
        ]


main : Html msg
main =
    div []
        ([ h3 [] [ text "LaTeX to Scripta Converter Tests" ] ]
            ++ List.map testOne tests
        )
