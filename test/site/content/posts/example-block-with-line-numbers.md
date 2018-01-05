+++
title = "Example blocks with line number annotation"
tags = ["example"]
draft = false
+++

-   [Org reference](https://orgmode.org/manual/Literal-examples.html)
-   [Hugo `highlight` shortcode with line numbers](https://gohugo.io/content-management/syntax-highlighting/)


## Default new line number start {#default-new-line-number-start}

{{< highlight text "linenos=table, linenostart=1">}}
line 1
 line 2
{{< /highlight >}}


## Specify new line number start {#specify-new-line-number-start}

{{< highlight text "linenos=table, linenostart=20">}}
line 20
line 21
{{< /highlight >}}


## Default continued line numbers {#default-continued-line-numbers}

{{< highlight text "linenos=table, linenostart=22">}}
 line 22
line 23
{{< /highlight >}}


## Specify continued line numbers jump {#specify-continued-line-numbers-jump}

{{< highlight text "linenos=table, linenostart=33">}}
line 33
line 34
{{< /highlight >}}
