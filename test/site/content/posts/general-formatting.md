+++
title = "General"
date = 2017-07-31
tags = ["formatting"]
draft = false
+++

Below table shows the translation of Org markup to Markdown markup in
the exported `.md` files.

| Org                | Markdown                                                             | In Hugo rendered HTML                    |
|--------------------|----------------------------------------------------------------------|------------------------------------------|
| `*bold*`           | `**bold**`                                                           | **bold**                                 |
| `/italics/`        | `_italics_`                                                          | _italics_                                |
| `=monospace=`      | `` `monospace` ``                                                    | `monospace`                              |
| `~key-binding~`    | `` `key-binding` ``                                                  | `key-binding`                            |
|                    | - if `org-hugo-use-code-for-kbd` is nil [default]                    |                                          |
| `~key-binding~`    | `<kbd>key-binding</kbd>`                                             |                                          |
|                    | - if `org-hugo-use-code-for-kbd` is non-nil                          |                                          |
|                    | - Requires **CSS** to render the `<kbd>` tag as something special.   |                                          |
| `+strike-through+` | `~~strike-through~~`                                                 | ~~strike-through~~                       |
| `_underline_`      | `<span class = "underline">underline</span>`                         | <span class="underline">underline</span> |
|                    | - Requires **CSS** to render this `underline` class as an underline. |                                          |
