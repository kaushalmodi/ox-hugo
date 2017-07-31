+++
title = "Inline equations"
date = 2017-07-31
tags = ["equations"]
draft = false
+++

Wrap the equations between `\(` and `\)`.

For example, below in Org:

```text
LaTeX formatted equation: \( E = -J \sum_{i=1}^N s_i s_{i+1} \)
```

will look like this in Hugo rendered HTML (don't see this in Markdown,
see what it looks after Hugo has processed it).

LaTeX formatted equation: \\( E = -J \sum\_{i=1}^N s\_i s\_{i+1 }\\)

Here's another example similar to one in [(org) LaTeX fragments](http://orgmode.org/manual/LaTeX-fragments.html).

If \\(a^2=b\\) and \\( b=2 \\), then the solution must be either
\\(a=+\sqrt{2}\\) or \\(a=-\sqrt{2}\\).
