+++
title = "Issue 382 – Newlines inserted before/after begin/end keywords of LaTeX equations"
description = "Markdown export issue creating extra newline when it is not necessary"
tags = ["issues", "latex", "equations"]
draft = false
+++

## `\left` and `\right` on the same lines as the rest of the equation {#left-and-right-on-the-same-lines-as-the-rest-of-the-equation}

\\[\left\\{\begin{align}
  \dot{x} & = \sigma(y-x) \newline
  \dot{y} & = \rho x - y - xz \newline
  \dot{z} & = -\beta z + xy
  \end{align} \right.\\]


## `\left` and `\right` by themselves on separate lines but with succeeding/preceding comments {#left-and-right-by-themselves-on-separate-lines-but-with-succeeding-preceding-comments}

\\[\left\\{ % but say if I insert comments here/random text
\begin{align}
  \dot{x} & = \sigma(y-x) \newline
  \dot{y} & = \rho x - y - xz \newline
  \dot{z} & = -\beta z + xy
  \end{align} % or random text here, problem goes away
\right.\\]


## `\left` and `\right` by themselves on separate lines {#left-and-right-by-themselves-on-separate-lines}

Due to an upstream bug in `ox-html.el`, below equation is not
rendering correctly at the moment --- `ox-hugo` Issue #[382](https://github.com/kaushalmodi/ox-hugo/issues/382)

\\[\left\\{

\begin{align}
  \dot{x} & = \sigma(y-x) \newline
  \dot{y} & = \rho x - y - xz \newline
  \dot{z} & = -\beta z + xy
  \end{align}

\right.\\]
