+++
title = "Blackfriday-specific escaping in equations"
description = "Test to check that the backslashes are correctly escaped."
tags = ["equations", "mathjax", "escaping", "blackfriday"]
categories = ["upstream"]
draft = false
+++

`ox-hugo` Issue #[138](https://github.com/kaushalmodi/ox-hugo/issues/138)


## `\|` → `\\|` {#}

\\[
C(w,b) = \frac{1}{2n} \sum\_x{{\\|y(x)-a\\|}^2}
\\]


## `\\` at EOL → `\\\\\\` {#at-eol}

\begin{align}
a^1  &= x \\\\\\
a^2  &=  σ(W^2a^1 + b^2) \\\\\\
a^3  &=  σ(W^3a^2 + b^3) \\\\\\
⋯ \\\\\\
a^L  &= σ(W^La^{L-1} + b^L) \\\\\\
y  &= a^L \\\\\\
\end{align}


### Same as above, but without space before the `\\` at EOL {#same-as-above-but-without-space-before-the-at-eol}

\begin{align}
a^1  &= x\\\\\\
a^2  &=  σ(W^2a^1 + b^2)\\\\\\
a^3  &=  σ(W^3a^2 + b^3)\\\\\\
⋯\\\\\\
a^L  &= σ(W^La^{L-1} + b^L)\\\\\\
y  &= a^L\\\\\\
\end{align}


## `\{` → `\\{`, `\}` → `\\}` {#}

`ox-hugo` Issue #[258](https://github.com/kaushalmodi/ox-hugo/issues/258)

\begin{equation}
\phi\_j(x) = \mathrm{exp}\left\\{ - \frac{(x - \mu\_j)^2}{2s^2} \right\\}
\end{equation}
