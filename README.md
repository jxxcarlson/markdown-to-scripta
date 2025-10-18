# Markdown to Scripta

Convert Markdown syntax to [Scripta](https://scripta.io) markup language.

There is only one exported function: `convert : String -> String`
in the module `MarkdownToScripta`.

## Installation

```bash
elm install jxxcarlson/markdown-to-scripta
```

## Usage

```elm
import MarkdownToScripta

markdown : String
markdown = """
# Hello World

This is **bold** and this is *italic*.

- Item 1
- Item 2

scripta : String
scripta = MarkdownToScripta.convert markdown

-- Result:
-- # Hello World
--
-- This is [b bold] and this is [i italic].
--
-- - Item 1
-- - Item 2
```

## Supported Markdown Features

See [the repo](https://github.com/jxxcarlson/markdown-to-scripta) if
the table below is not rendered properly.


| Markdown | Scripta | Example |
|----------|---------|---------|
| Headings | Same | `# Heading` → `# Heading` |
| Bold | `[b ...]` | `**bold**` → `[b bold]` |
| Italic | `[i ...]` | `*italic*` → `[i italic]` |
| Inline code | Same | `` `code` `` → `` `code` `` |
| Code blocks | `\| code` | ` ```python ` → `\| code python` |
| Links | `[link ...]` | `[text](url)` → `[link text url]` |
| Images | `[link ...]` | `![alt](url)` → `[link alt url]` |
| Blockquotes | `\| quotation` | `> quote` → `\| quotation` |
| Tables | `\| table` | Pipe tables → `\| table` format |
| Horizontal rule | `[hrule]` | `---` → `[hrule]` |
| Lists | Same | `- item` → `- item` |
| Inline math | Same | `$x^2$` → `$x^2$` |
| Display math | `\| equation` | `$$E=mc^2$$` → `\| equation` |

## Math Support

### Inline Math

Inline math uses single dollar signs with strict whitespace rules:
- Valid: `$x^2$`, `$E=mc^2$`
- Invalid: `$ x^2$` (space after `$`), `$x^2 $` (space before `$`)

### Display Math

Display math uses double dollar signs and converts to equation blocks:

```markdown
$$
E = mc^2
$$
```

Becomes:

```
| equation
E = mc^2
```

## Demo Application

A live demo application is included in the `demo/` directory:

```bash
cd demo/
sh make.sh
```

Then open `build/index.html` in your browser.


## License

MIT
