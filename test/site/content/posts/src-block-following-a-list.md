+++
title = "Source block following a list"
description = """
  Test to verify rendering of a source block immediately following a
  plain list, and even a list following a heading following a source
  block.
  """
tags = ["lists", "src-block"]
draft = false
+++

[Ref](https://discourse.gohugo.io/t/rendering-code-blocks-properly-from-md-files/19126)

-   list item 1
-   list item 2

```nim
echo "hello"
```

-   another list item 1
-   another list item 2


## A heading in post {#a-heading-in-post}

```nim
echo "hello again"
```
