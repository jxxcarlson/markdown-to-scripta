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
      , expected = ". First\n. Second\n. Third"
      }
    , { name = "Verbatim block"
      , latex = "\\begin{verbatim}\nfor i in range(5):\n    print(i)\n\\end{verbatim}"
      , expected = "| code\n  for i in range(5):\n      print(i)"
      }
    , { name = "Lstlisting block"
      , latex = "\\begin{lstlisting}\ndef hello():\n    print('Hello, world!')\n\\end{lstlisting}"
      , expected = "| code\n  def hello():\n      print('Hello, world!')"
      }
    , { name = "Equation environment"
      , latex = "\\begin{equation}\na^2 + b^2 = c^2\n\\end{equation}"
      , expected = "| equation\na^2 + b^2 = c^2"
      }
    , { name = "Ordinary block (theorem)"
      , latex = "\\begin{theorem}\nThere are infinitely many primes.\n\\end{theorem}"
      , expected = "| theorem\n  There are infinitely many primes."
      }
    , { name = "Debug - minimal theorem"
      , latex = "\\begin{theorem}\ntest\n\\end{theorem}"
      , expected = "| theorem\n  test"
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
    , { name = "Environment with properties"
      , latex = "\\begin{theorem}[leftmargin=2em,label=\\arabic*,blahblah]\nThere are infinitely many primes.\n\\end{theorem}"
      , expected = "| theorem leftmargin:2em label:\\arabic* blahblah\n  There are infinitely many primes."
      }
    , { name = "Lists with properties"
      , latex = "\\begin{itemize}[leftmargin=2em]\n\\item Bullet one.\n\\item Bullet two with inline math $a^2+b^2=c^2$.\n\\end{itemize}\n\\begin{enumerate}[leftmargin=2em,label=\\arabic*)]\n\\item First.\n\\item Second.\n\\item Third.\n\\end{enumerate}"
      , expected = "| item leftmargin:2em\n- Bullet one.\n- Bullet two with inline math $a^2+b^2=c^2$.\n\n| numbered leftmargin:2em label:\\arabic*)\n. First.\n. Second.\n. Third."
      }
    , { name = "Equation with label"
      , latex = "\\begin{equation}\n\\label{eq:newton}\nm \\ddot{\\vb x}(t) = -\\nabla V\\!\\bigl(\\vb x(t)\\bigr).\n\\end{equation}"
      , expected = "| equation label:eq:newton\nm \\ddot{\\vb x}(t) = -\\nabla V\\!\\bigl(\\vb x(t)\\bigr)."
      }
    , { name = "Align environment"
      , latex = "\\begin{align}\n\\int_{0}^{1} x^n \\, dx &= \\frac{1}{n+1}, \\qquad n>-1,\\\\\n\\sum_{k=1}^{\\infty} \\frac{1}{k^2} &= \\frac{\\pi^2}{6}.\n\\end{align}"
      , expected = "| aligned\n  \\int_{0}^{1} x^n \\, dx &= \\frac{1}{n+1}, \\qquad n>-1,\\\\\n  \\sum_{k=1}^{\\infty} \\frac{1}{k^2} &= \\frac{\\pi^2}{6}."
      }
    , { name = "Theorem with nested equation"
      , latex = "\\begin{theorem}[Cauchy--Schwarz]\nFor all $\\vb x,\\vb y\\in V$,\n\\begin{equation}\n|\\langle \\vb x,\\vb y\\rangle| \\le \\|\\vb x\\|\\;\\|\\vb y\\|.\n\\end{equation}\n\\end{theorem}"
      , expected = "| theorem Cauchy--Schwarz\n  For all $\\vb x,\\vb y\\in V$,\n\n  | equation\n  |\\langle \\vb x,\\vb y\\rangle| \\le \\|\\vb x\\|\\;\\|\\vb y\\|."
      }
    , { name = "Table with caption and label"
      , latex = "\\begin{table}[h!]\n\\centering\n\\begin{tabular}{l S[table-format=2.2] S[table-format=1.3]}\n\\toprule\n{Method} & {Accuracy (\\%)} & {Time (s)}\\\\\n\\midrule\nBaseline & 72.45 & 0.153\\\\\nModel A  & 83.10 & 0.982\\\\\nModel B  & 85.25 & 1.247\\\\\n\\bottomrule\n\\end{tabular}\n\\caption{Example results}\n\\label{tab:ex}\n\\end{table}"
      , expected = "| table caption:Example results label:tab:ex table-format:h!\nBaseline & 72.45 & 0.153\nModel A  & 83.10 & 0.982\nModel B  & 85.25 & 1.247"
      }
    , { name = "TikZ picture"
      , latex = "\\begin{tikzpicture}[scale=1.0]\n  % Axes\n  \\draw[->] (-0.2,0) -- (4.2,0) node[right] {$x$};\n  \\draw[->] (0,-0.2) -- (0,3.2) node[above] {$y$};\n\\end{tikzpicture}"
      , expected = "| tikz get:image scale:1.0\n\\begin{tikzpicture}\n  % Axes\n  \\draw[->] (-0.2,0) -- (4.2,0) node[right] {$x$};\n  \\draw[->] (0,-0.2) -- (0,3.2) node[above] {$y$};\n\\end{tikzpicture}"
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
