+++
title = "Citations with captions"
date = 2018-08-19T16:24:00-00:00
tags = ["pandoc", "citations"]
draft = false
+++

`ox-hugo` Issue
\#[191](https://github.com/kaushalmodi/ox-hugo/issues/191)

## Why an electronic lab notebook?

At an age where data is moving from paper to electronic form, the amount
and **shape** of data is not very paper-compatible, and the analysis of
that same data becomes more complex, it is just obvious that the move to
an electronic notebook has to happen.

## Why use Org mode?

Check out the code block in [1](#org4bfd6da).

## Using code blocks

<a id="org4bfd6da"></a>

``` plantuml
rectangle "<html>, <body>, etc." as a  {
  rectangle "<div>..." as b #antiquewhite {
    rectangle "<video>...\n\n\n" as c
  }
}
```

<div class="src-block-caption">

<span class="src-block-number">Code Snippet 1:</span> Nested Boxes

</div>

{{< figure src="nested-boxes.svg" >}}

## Figure with caption

{{< figure src="/images/org-mode-unicorn-logo.png" caption="Figure 1: A
unicorn!" >}}

## References {#references}

<div id="refs" class="references">
  <div></div>


<div id="ref-eilan2016">
  <div></div>

Eilan, Naomi. 2016. "You Me and the World." *Analysis* 76 (3): 311--24.

</div>

<div id="ref-giovanelli2016">
  <div></div>

Giovanelli, Marco. 2016. "\"\...But I Still Can't Get Rid of a Sense of
Artificiality\" the Reichenbach--Einstein Debate on the Geometrization
of the Electromagnetic Field." *Studies in History and Philosophy of
Science* 54: 35--51.

</div>

</div>
