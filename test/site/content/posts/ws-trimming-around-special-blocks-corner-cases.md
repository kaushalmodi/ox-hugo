+++
title = "Whitespace trimming around special blocks (corner cases)"
description = """
  Corner cases for testing the whitespace trimming around the special
  blocks.
  """
tags = ["special-block", "whitespace", "trimming", "corner-cases"]
draft = false
+++

## Whitespace trimming inside quote blocks {#whitespace-trimming-inside-quote-blocks}

> line 1
<mark>marked text</mark>
line 2
>
> line 4 (line 3, above, is blank, but inside the quote block)


## Whitespace trimming before list elements {#whitespace-trimming-before-list-elements}

something
<mark>marked text</mark>
-   list item 1
-   list item 2


## Whitespace trimming before headings {#whitespace-trimming-before-headings}

something
<mark>marked text</mark>


### Next heading {#next-heading}


## Whitespace trimming before and after code blocks {#whitespace-trimming-before-and-after-code-blocks}

something
<mark>marked text</mark>
```text
code line
```
<mark>marked text</mark>
something
`inline code 1`
<mark>marked text</mark>
`inline code 2`
something


## Whitespace trimming with `:trim-pre` set immediately after a heading {#whitespace-trimming-with-trim-pre-set-immediately-after-a-heading}
<mark>marked text</mark>


## Last element of a post {#last-element-of-a-post}
<mark>No "post" trim markers should be inserted after this block as it's the
last element of this post.</mark>
