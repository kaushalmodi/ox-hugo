+++
title = "Citation Forms (Custom CSL)"
description = "Demonstrating different styles / forms of citations using APA CSL"
tags = ["citations", "pandoc", "forms", "csl"]
draft = false
+++

## Citations in square brackets

The citations withing square brackets will be rendered within
parentheses.

``` org
Blah blah [see @doe99, pp. 33-35; also @smith04, ch. 1].
```

Blah blah (see [Doe, 1999, pp. 33--35](#ref-doe99); also [Smith,
2004](#ref-smith04), ch. 1).

``` org
Blah blah [@doe99, pp. 33-35, 38-39].
```

Blah blah ([Doe, 1999, pp. 33--35, 38--39](#ref-doe99)).

``` org
Blah blah [@smith04; @doe99].
```

Blah blah ([Doe, 1999](#ref-doe99); [Smith, 2004](#ref-smith04)).

## Citations with author name suppressed

A minus sign (`-`) before the `@` will suppress mention of the author in
the citation. This can be useful when the author is already mentioned in
the text.

``` org
Smith says blah [-@smith04].
```

Smith says blah ([2004](#ref-smith04)).

## In-text citations (no square brackets) {#in-text-citations--no-square-brackets}

``` org
@smith04 says blah.
```

Smith ([2004](#ref-smith04)) says blah.

``` org
@smith04 [p. 33] says blah.
```

Smith ([2004, p. 33](#ref-smith04)) says blah.

## Actual citations for this test post :)

See Xie ([2017](#ref-addCite17)); "Bibliographies and Citations /
Citations" ([n.d.](#ref-rmdCitations)) for more.

**Compare the References section below with
[that](/posts/citation-forms/#references) when using the default CSL.**

## References {#references}

<div id="refs" class="references csl-bib-body hanging-indent"
line-spacing="2">

<div id="ref-rmdCitations" class="csl-entry">

Bibliographies and citations / citations. (n.d.). In *RMarkdown*.
RStudio.
<https://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html#citations>

</div>

<div id="ref-doe99" class="csl-entry">

Doe, J. (1999). *An awesome revelation* (3rd ed., Vol. 4). Publisher
Foo.

</div>

<div id="ref-smith04" class="csl-entry">

Smith, A. (2004). *The book on life* (1st ed., Vol. 7). Publisher Bar.

</div>

<div id="ref-addCite17" class="csl-entry">

Xie, Y. (2017). *Adding citations to posts*.
<https://blogdown-demo.rbind.io/2017/08/28/adding-citations-to-posts/>

</div>

</div>
