+++
title = "Citations with captions"
description = """
  This test tests the following:

  -   Pandoc leave the HTML `span` tags as-is.
  -   Pandoc does not escape the `<` in the `figure` shortcodes with
      captions (in general: `{{< ..>}}` shortcodes that could wrap across
      lines).
  -   While Pandoc auto-wraps the re-written Markdown, it also wraps the
      `{{< .. >}}` shortcodes. The test checks that such "wrapped
      shortcodes" get unwrapped.
  """
date = 2018-08-19
tags = ["pandoc", "citations", "caption", "figure", "plantuml"]
draft = false
+++

`ox-hugo` Issue
\#[191](https://github.com/kaushalmodi/ox-hugo/issues/191)

<a id="code-snippet-1"></a>

``` plantuml
rectangle "<html>, <body>, etc." as a  {
  rectangle "<div>..." as b #antiquewhite {
    rectangle "<video>...\n\n\n" as c
  }
}
```

<div class="src-block-caption">

<span class="src-block-number"><a href="#code-snippet-1">Code Snippet
1</a></span>: Nested Boxes using PlantUML

</div>

{{< figure src="nested-boxes.svg" caption="Figure 1: PlantUML generated figure showing nested boxes" >}}

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
