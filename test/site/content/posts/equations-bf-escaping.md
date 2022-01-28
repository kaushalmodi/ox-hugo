+++
title = "Blackfriday-specific escaping in equations"
description = "Test to check that the backslashes are correctly escaped."
tags = ["equations", "mathjax", "escaping", "blackfriday"]
categories = ["upstream"]
draft = false
+++

`ox-hugo` Issue #[138](https://github.com/kaushalmodi/ox-hugo/issues/138)


## `\|` → `\\|` {#9d819a}

\\[
C(w,b) = \frac{1}{2n} \sum\_x{{\\|y(x)-a\\|}^2}
\\]


## `\\` at EOL → `\\\\` {#at-eol}

\begin{align}
a^1  &= x \\\\
a^2  &=  σ(W^2a^1 + b^2) \\\\
a^3  &=  σ(W^3a^2 + b^3) \\\\
⋯ \\\\
a^L  &= σ(W^La^{L-1} + b^L) \\\\
y  &= a^L
\end{align}


### Same as above, but without space before the `\\` at EOL {#same-as-above-but-without-space-before-the-at-eol}

\begin{align}
a^1  &= x\\\\
a^2  &=  σ(W^2a^1 + b^2)\\\\
a^3  &=  σ(W^3a^2 + b^3)\\\\
⋯\\\\
a^L  &= σ(W^La^{L-1} + b^L)\\\\
y  &= a^L
\end{align}


## `\{` → `\\{`, `\}` → `\\}` {#3d16e4}

`ox-hugo` Issue #[258](https://github.com/kaushalmodi/ox-hugo/issues/258)

\begin{equation}
\phi\_j(x) = \mathrm{exp}\left\\{ - \frac{(x - \mu\_j)^2}{2s^2} \right\\}
\end{equation}


## `x <0 \\` {#x-0}

`ox-hugo` Issue #[348](https://github.com/kaushalmodi/ox-hugo/issues/348)

\begin{equation}
\begin{cases}
u\_t = ku\_{xx} \\\\
u(x,0) = T\_1 , & x <0 \\\\
u(x,0) = T\_2 , & x > 0
\end{cases}
\end{equation}


## `[ .. ]( .. )` in a LaTeX equation {#dot-dot--dot-dot--in-a-latex-equation}

`ox-hugo` Issue #[349](https://github.com/kaushalmodi/ox-hugo/issues/349)

In the below equation, without the escaping hack, the Markdown parser
gets fooled into thinking that `[ e^{at} \right](z)` is a Markdown
link!

\begin{equation}
\mathcal{L}\left[ e^{at} \right\]\(z) = \frac{1}{z-a}
\end{equation}


## Backslashes within a LaTeX equation {#backslashes-within-a-latex-equation}

`ox-hugo` Issue #[458](https://github.com/kaushalmodi/ox-hugo/issues/458)

Double backslashes (`\\`) should be escaped:
\\[\sum\_{\substack{0<i<m\\\0<j<n}}\\]

A backslash before any ASCII punctuation character should be escaped,
for example, `\,`, `\;`, `\:`, `\!` are used to control the width of
spacing:
  \\[ab\\]
  \\[a\ b\\]
  \\[a\\,b\\]
  \\[a\\;b\\]
  \\[a\\:b\\]
  \\[a\\!b\\]
