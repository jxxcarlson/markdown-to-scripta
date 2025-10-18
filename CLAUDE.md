# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Markdown to Scripta converter written in Elm. It parses Markdown syntax into an AST (Abstract Syntax Tree) and renders it as Scripta markup language.

## Build Commands

```bash
# Build the application
sh make.sh

# Compile directly (equivalent to make.sh)
elm make src/Main.elm --output=build/elm.js
```

The build output is `build/elm.js`, which can be included in an HTML page.

## Architecture

The codebase follows a clean three-stage pipeline architecture:

```
Markdown text → Parser → AST → Renderer → Scripta markup
```

### Module Structure

**src/Markdown/AST.elm**
- Defines the Abstract Syntax Tree types
- Two main types: `Block` (paragraphs, headings, lists, tables, code blocks, math blocks, blockquotes, horizontal rules) and `Inline` (plain text, bold, italic, code, math, links, images)
- `Document` is a list of `Block`
- `ListItem` contains indent level, ordered/unordered flag, optional number, and inline content

**src/Markdown/Parser.elm**
- Uses `elm/parser` to parse Markdown into AST
- Main entry point: `parse : String -> Result (List DeadEnd) Document`
- Parser combinators build up from inline elements to block elements
- Block parsers are tried in order (headings, horizontal rules, math blocks, code blocks, blockquotes, tables, lists, paragraphs)
- Inline parsers handle nested content (images, links, bold, italic, math, code, plain text)

**src/Markdown/Renderer.elm**
- Converts AST to Scripta markup
- Main entry point: `render : Document -> String`
- Simple pattern matching on AST nodes to produce Scripta syntax

**src/MarkdownToScripta.elm**
- Public API module that combines Parser and Renderer
- `convert : String -> String` - returns original text on parse error

**src/Main.elm**
- Elm Browser.element application with a simple two-panel UI
- Left panel: Markdown input textarea
- Right panel: Scripta output textarea
- Convert button triggers the conversion

## Parser Implementation Details

### Performance Considerations

**IMPORTANT**: Avoid `backtrackable` whenever possible. Look ahead using `chompIf` or similar primitives instead. Backtracking makes parsers potentially much slower.

### Math Parsing

Inline math (`$...$`) has strict whitespace rules:
- First `$` must NOT be followed by whitespace
- Last `$` must NOT be preceded by whitespace
- Valid: `$x^2$`
- Invalid: `$ x^2$` or `$x^2 $`

Display math (`$$...$$`) can be inline or multiline and converts to `| equation\n<content>`.

### Loop Parsers

Parser helpers using `loop` must ensure at least one element is consumed before entering the loop to avoid infinite loops. For example, `listParser` requires parsing `firstItem` before calling `loop [ firstItem ] listHelper`.

### Common Patterns

- Use `getChompedString` to capture parsed content as a string
- Use `chompUntil` instead of loop-based parsing for delimited content (e.g., code blocks with ``` delimiters)
- Use `String.split` for simple tabular data instead of complex backtracking parsers (e.g., table cells split by `|`)
- Inline parsers are tried in order; put more specific parsers (like images `![...]`) before less specific ones (like links `[...]`)

## Markdown to Scripta Conversion Rules

- Headings: Same syntax (`#`, `##`, etc.)
- Bold: `**text**` → `[b text]`
- Italic: `*text*` → `[i text]`
- Code: `` `code` `` → `` `code` `` (unchanged)
- Code blocks: ` ```lang ` → `| code lang`
- Links: `[text](url)` → `[link text url]`
- Images: `![alt](url)` → `[link alt url]` (or `[link image url]` if alt is empty)
- Blockquotes: `> text` → `| quotation`
- Tables: Pipe-separated rows → `| table` with `&` separators and `\\` row endings
- Horizontal rule: `---` → `[hrule]`
- Inline math: `$x^2$` → `$x^2$` (unchanged)
- Display math: `$$E=mc^2$$` → `| equation\nE=mc^2`
- Lists: Unordered (`-`, `*`, `+`) and ordered (`1.`) with indent support

## Development Workflow

1. Make changes to source files in `src/`
2. Run `sh make.sh` to compile
3. Open an HTML file that loads `build/elm.js` to test
4. If parser changes are made, consider edge cases like:
   - Missing closing delimiters
   - Empty content
   - Whitespace variations
   - Nested structures
