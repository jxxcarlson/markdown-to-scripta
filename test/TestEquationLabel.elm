module TestEquationLabel exposing (main)

import Html exposing (Html, div, h3, pre, text)
import LaTeXToScripta

input : String
input =
    """\\begin{equation}
\\label{eq:newton}
m \\ddot{\\vb x}(t) = -\\nabla V\\!\\bigl(\\vb x(t)\\bigr).
\\end{equation}"""

output : String
output =
    LaTeXToScripta.convert input

main : Html msg
main =
    div []
        [ h3 [] [ text "Equation with Label Test" ]
        , pre [] [ text "Input:" ]
        , pre [] [ text input ]
        , pre [] [ text "\nOutput:" ]
        , pre [] [ text output ]
        , pre [] [ text "\nExpected:" ]
        , pre [] [ text "| equation label:eq:newton\nm \\ddot{\\vb x}(t) = -\\nabla V\\!\\bigl(\\vb x(t)\\bigr)." ]
        ]
