+++
title = "Equations with (r), (c), .."
description = "Test to check that &reg;, &copy; and &trade; don't get interpreted within equations."
tags = ["equations", "mathjax"]
categories = ["upstream"]
draft = false
+++

`ox-hugo` Issue #[104](https://github.com/kaushalmodi/ox-hugo/issues/104)

Below, `(r)` or `(R)` should not get converted to &reg;, `(c)` or `(C)`
should not get converted to &copy;, and `(tm)` or `(TM)` should not get
converted to &trade;:

-   \\(( r)\\) \\(( R)\\)
-   \\(( c)\\) \\(( C)\\)
-   \\(( tm)\\) \\(( TM)\\)

<!--listend-->

-   \\( ( r) \\) \\( ( R) \\)
-   \\( ( c) \\) \\( ( C) \\)
-   \\( ( tm) \\) \\( ( TM) \\)

Same as above but in _Block Math equations_:

\\[ ( r) ( R) \\]
\\[ ( c) ( C) \\]
\\[ ( tm) ( TM) \\]

\\[ ( r) ( R) \\]
\\[ ( c) ( C) \\]
\\[ ( tm) ( TM) \\]
