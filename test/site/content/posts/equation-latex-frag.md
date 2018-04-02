+++
title = "Inline equations"
description = "Inline and _one-per-line_ equations"
date = 2017-07-31
tags = ["equations", "mathjax"]
draft = false
+++

-   Inline equations are wrapped between `\(` and `\)`.
    -   `$` wrapping also works, but it is not preferred as it comes with
        restrictions like "there should be no whitespace between the
        equation and the `$` delimiters".

        So `$ a=b $` will not work (it will look like: $ a=b $), but
        `$a=b$` will work (it will look like: \\(a=b\\)).

        On the other hand, both `\(a=b\)` (it will look like: \\(a=b\\)) and
        `\( a=b \)` (it will look like: \\( a=b \\)) will work.
-   One-per-line equations are wrapped between `\[` and `\]` or `$$`
    delimiters.

For example, below in Org:

```text
LaTeX formatted equation: \( E = -J \sum_{i=1}^N s_i s_{i+1} \)
```

will look like this in Hugo rendered HTML:

LaTeX formatted equation: \\( E = -J \sum\_{i=1}^N s\_i s\_{i+1 }\\)

(Don't see this in Markdown, see what it looks after Hugo has
processed it.)

Here's another example, taken from [(org) LaTeX fragments](https://orgmode.org/manual/LaTeX-fragments.html).

Below in Org:

```text
If $a^2=b$ and \( b=2 \), then the solution must be either
$$ a=+\sqrt{2} $$ or \[ a=-\sqrt{2} \]
```

renders to:

If \\(a^2=b\\) and \\( b=2 \\), then the solution must be either
\\[ a=+\sqrt{2} \\] or \\[ a=-\sqrt{2} \\]

(Note that the last two equations show up on their own lines.)
