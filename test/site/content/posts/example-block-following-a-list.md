+++
title = "Example block following a list"
description = """
  Test to verify rendering of a example block immediately following a
  plain list, and even a list following a heading following a example
  block.
  """
tags = ["lists", "example-block"]
categories = ["upstream"]
draft = false
+++

-   [Ref 1](https://discourse.gohugo.io/t/rendering-code-blocks-properly-from-md-files/19126)
-   [Ref 2](https://discourse.gohugo.io/t/possible-regression-in-v0-55-5-regarding-lists-containing-code-blocks/18502/4?u=kaushalmodi)
-   _Blackfriday_ Issue #[556](https://github.com/russross/blackfriday/issues/556)

<!--listend-->

-   list item 1
-   list item 2

<!--listend-->

```text
something in example block
```

-   another list item 1
-   another list item 2


## A heading in post {#a-heading-in-post}

```text
another example block
```
