+++
title = "Indented equations block"
description = "Testing equations with mathjax --- indented or not."
tags = ["equations", "mathjax", "indented"]
draft = false
+++

## No indentation {#no-indentation}

\begin{equation}
\label{eq:1}
C = W\log\_{2} (1+\mathrm{SNR})
\end{equation}


## With indentation {#with-indentation}

`ox-hugo` Issue #[128](https://github.com/kaushalmodi/ox-hugo/issues/128)

\begin{equation}
\label{eq:2}
C = W\log\_{2} (1+\mathrm{SNR})
\end{equation}

Above equation (_<span class="latex">L<sup>a</sup>T<sub>e</sub>X</span> environment_) is the same as the first
one, but:

-   It is indented in the Org source.

    _This test verifies that the indentation is auto-removed in the
    exported Markdown file._
-   It has a different label (`\label{eq:2}` instead of `\label{eq:1}`);
    Mathjax **requires the equation labels to be unique**.
