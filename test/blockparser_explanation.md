# How blockParser Works in the LaTeX Parser

## Overview

`blockParser` is responsible for parsing a single block-level element in LaTeX. It's the core parser that identifies and dispatches to specific parsers for different block types.

## Structure

```elm
blockParser : Parser Block
blockParser =
    oneOf
        [ sectionParser        -- tries to parse \section, \subsection, etc.
        , environmentParser    -- tries to parse \begin{name}...\end{name}
        , paragraphParser      -- tries to parse paragraph text
        ]
```

The `oneOf` combinator tries each parser in sequence. If one fails, it backtracks and tries the next one. If all fail, the whole `blockParser` fails.

## Detailed Trace: Parsing a Theorem Block

Let's trace through what happens when parsing:
```latex
\begin{theorem}
There are infinitely many primes.
\end{theorem}
```

### Step 1: Document Level

`documentHelper` calls `blockParser` to parse the first block.

### Step 2: blockParser Attempts

1. **Tries sectionParser:** Fails (input doesn't start with `\section`)
2. **Tries environmentParser:** SUCCESS

### Step 3: environmentParser

```elm
environmentParser : Parser Block
environmentParser =
    succeed identity
        |. symbol "\\begin"
        |. spaces
        |. symbol "{"
        |= getChompedString (chompUntil "}")
        |. symbol "}"
        |. spaces
        |. oneOf [ symbol "\n", succeed () ]
        |> andThen (\envName -> ...)
```

Actions:
- Matches `\begin`
- Extracts `"theorem"` as `envName`
- Matches the braces `{` and `}`
- Consumes spaces and optional newline
- Dispatches to `ordinaryBlockParser "theorem"` (since "theorem" is not itemize, enumerate, verbatim, equation, or code)

### Step 4: ordinaryBlockParser

```elm
ordinaryBlockParser : Name -> Parser Block
ordinaryBlockParser envName =
    loop [] (ordinaryBlockHelper envName)
        |> map (OrdinaryBlock envName)
```

This starts a loop with an empty list of blocks `[]`.

### Step 5: First Iteration of ordinaryBlockHelper

**Current position in input:** `"There are infinitely many primes.\n\end{theorem}"`

```elm
ordinaryBlockHelper : Name -> List Block -> Parser (Step (List Block) (List Block))
ordinaryBlockHelper envName blocks =
    succeed identity
        |. spaces                -- consumes any whitespace
        |> andThen
            (\_ ->
                oneOf
                    [ succeed ()
                        |. symbol "\\end"
                        |. spaces
                        |. symbol "{"
                        |. token envName
                        |. symbol "}"
                        |. oneOf [ symbol "\n", end ]
                        |> map (\_ -> Done (List.reverse blocks))
                    , succeed (\block -> Loop (block :: blocks))
                        |= blockParser
                    ]
            )
```

Actions:
- Consumes any leading spaces (none in this case)
- Tries the `oneOf`:
  - **First option:** Try to match `\end{theorem}`
    - Looks for `\end` - **FAILS** (next character is `T` from "There")
  - **Second option:** Parse a block via recursive `blockParser` call
    - **PROCEEDS** to Step 6

### Step 6: Recursive blockParser Call (Inside Theorem)

The recursive `blockParser` tries:
1. **sectionParser** - fails (no `\section`)
2. **environmentParser** - fails (no `\begin`)
3. **paragraphParser** - **SUCCEEDS**

### Step 7: paragraphParser

```elm
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
                        |> andThen
                            (\first ->
                                loop [ first ] paragraphHelper
                                    |> andThen
                                        (\lines ->
                                            parseInlinesFromString (String.join " " lines)
                                                |> map Paragraph
                                        )
                            )
            )
```

Actions:
- Chomps `"There are infinitely many primes."` (stops at `\n`)
- Verifies the line is not empty
- Consumes the `\n`
- Enters `paragraphHelper` loop with `["There are infinitely many primes."]`

### Step 8: paragraphHelper Loop

**Current position in input:** `"\end{theorem}"`

```elm
paragraphHelper : List String -> Parser (Step (List String) (List String))
paragraphHelper lines =
    oneOf
        [ -- Check if next character is backslash (start of command/environment)
          backtrackable (symbol "\\")
            |> map (\_ -> Done (List.reverse lines))
        , getChompedString (chompUntilEndOr "\n")
            |> andThen (\line -> ...)
        , succeed (Done (List.reverse lines))
        ]
```

Actions:
- **First option:** Looks for `\` - **MATCHES!** (because `\end` starts with `\`)
- Returns `Done` with the collected lines: `["There are infinitely many primes."]`
- The paragraph content is parsed as inlines and wrapped in a `Paragraph` block

### Step 9: Back to ordinaryBlockHelper

The recursive `blockParser` succeeded and returned:
```elm
Paragraph [Text "There are infinitely many primes."]
```

The loop continues with:
- `Loop` with `block :: blocks` = `[Paragraph [...]]`
- Proceeds to second iteration

### Step 10: Second Iteration of ordinaryBlockHelper

**Current position in input:** `"\end{theorem}"`

Actions:
- Consumes spaces (none)
- Tries `oneOf`:
  - **First option:** Try to match `\end{theorem}`
    - Matches `\end` ✓
    - Consumes spaces (none)
    - Matches `{` ✓
    - Matches token `"theorem"` ✓
    - Matches `}` ✓
    - Matches `\n` or reaches `end` ✓
    - Returns `Done` with reversed blocks: `[Paragraph [...]]`

### Step 11: Back to ordinaryBlockParser

Maps the result to:
```elm
OrdinaryBlock "theorem" [Paragraph [Text "There are infinitely many primes."]]
```

### Step 12: Rendering

The renderer converts this AST to Scripta format:
```
| theorem
There are infinitely many primes.
```

## Why It's Currently Failing

The trace above shows what SHOULD happen. The fact that the test returns the original LaTeX unchanged means the entire parse is failing at the document level.

The `documentParser` looks like:
```elm
documentParser : Parser Document
documentParser =
    succeed identity
        |. spaces
        |= loop [] documentHelper
        |. end  -- requires we're at end of input
```

After parsing all blocks, it requires `|. end` - meaning all input must be consumed. If there's any trailing content that wasn't consumed, the entire parse fails.

### Hypothesis

The issue may be in how `ordinaryBlockHelper` consumes the final `\end{theorem}` line. Looking at line 192:

```elm
|. oneOf [ symbol "\n", end ]
```

For the test input `\begin{theorem}\nThere are infinitely many primes.\n\end{theorem}`, there's **no trailing newline** after the final `}`.

The parser tries to match:
- `symbol "\n"` - fails (no newline)
- `end` - should succeed if we're at EOF

The `end` parser succeeds only when at end-of-file, which we should be. So this SHOULD work.

### Next Steps for Debugging

1. Add a simpler test case to isolate where parsing fails
2. Add debug logging to see which parser is failing
3. Check if there's an issue with how the `end` parser works in this context
4. Verify that `token` properly matches the environment name
