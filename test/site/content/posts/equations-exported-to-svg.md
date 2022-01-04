+++
title = "Equations exported to SVG (dvisvgm)"
description = "Exporting <span class=\"latex\">L<sup>a</sup>T<sub>e</sub>X</span> equations as SVG images."
tags = ["equations", "dvisvgm", "dont-export-during-make-test"]
draft = false
+++

`ox-hugo` Issue #[327](https://github.com/kaushalmodi/ox-hugo/issues/327)

Example of an inline equation: <img src="/ltximg/all-posts_ceee782e7b4ae762ba5504ce2f6f7515c733c34c.svg" alt="\[ a + b \]" class="org-svg" />

Example of a block equation:

<img src="/ltximg/all-posts_bfe9e5d17c9e7a6e0fb64dba06c0d3466b8d9542.svg" alt="\begin{equation*}
C = W\log_{2} (1+\mathrm{SNR})
\end{equation*}
" class="org-svg" />
</span>
<span class="equation-label">
1
</span>
</div>

<div class="note">

Referencing to equation labels does not work when <span class="latex">L<sup>a</sup>T<sub>e</sub>X</span>
equations are exported as images.

</div>
