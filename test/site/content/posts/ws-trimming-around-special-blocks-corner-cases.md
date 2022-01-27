+++
title = "Whitespace trimming around special blocks (corner cases)"
tags = ["special-block", "whitespace", "trimming"]
draft = false
+++

## Whitespace trimming inside quote blocks {#whitespace-trimming-inside-quote-blocks}

> line 1 <mark>marked text</mark> line 2


## Whitespace trimming before list elements {#whitespace-trimming-before-list-elements}

something <mark>marked text</mark>
-   list item 1
-   list item 2


## Whitespace trimming before headings {#whitespace-trimming-before-headings}

something <mark>marked text</mark>
### Next heading {#next-heading}


## Whitespace trimming before and after code blocks {#whitespace-trimming-before-and-after-code-blocks}

something <mark>marked text</mark>
```text
code line
```
 <mark>marked text</mark> something
`inline code 1`
 <mark>marked text</mark>
`inline code 2`