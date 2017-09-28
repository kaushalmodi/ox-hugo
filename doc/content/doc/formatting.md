+++
title = "Formatting"
draft = false
[menu."org_to_hugo"]
  weight = 3002
  identifier = "formatting"
+++

Below table shows the translation of Org markup to Markdown markup in
the exported `.md` files.

See the Org source in [`all-posts.org`](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content-org/all-posts.org) under _Formatting_ -> _General_
heading and how it exports to Markdown in [`general-formatting.md`](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content/posts/general-formatting.md).

| Org                | Markdown                                                             |
|--------------------|----------------------------------------------------------------------|
| `*bold*`           | `**bold**`                                                           |
| `/italics/`        | `_italics_`                                                          |
| `=monospace=`      | `` `monospace` ``                                                    |
| `~key-binding~`    | `` `key-binding` ``                                                  |
|                    | - if `org-hugo-use-code-for-kbd` is nil [default]                    |
| `~key-binding~`    | `<kbd>key-binding</kbd>`                                             |
|                    | - if `org-hugo-use-code-for-kbd` is non-nil                          |
|                    | - Requires **CSS** to render the `<kbd>` tag as something special.   |
| `+strike-through+` | `~~strike-through~~`                                                 |
| `_underline_`      | `<span class = "underline">underline</span>`                         |
|                    | - Requires **CSS** to render this `underline` class as an underline. |
