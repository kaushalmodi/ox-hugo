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
<mark>marked text</mark> line 2
>
> line 4 (line 3, above, is blank, but inside the quote block)

-   Special block in a quote block in a list

    > line 1
<mark>marked text</mark> line 2
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
<mark>marked text</mark> something
`inline code 1`
<mark>marked text</mark>
`inline code 2`
something


## Whitespace trimming with `:trim-pre` set immediately after a heading {#whitespace-trimming-with-trim-pre-set-immediately-after-a-heading}
<mark>marked text</mark>


## `>` character before a special block with `:trim_pre` set {#character-before-a-special-block-with-trim-pre-set}

<style>
  .red {
    color: red;
  }
</style>
<mark class="red">This marked text's foreground is red.</mark>


## Hugo shortcode in a definition list with pre/post trimming {#hugo-shortcode-in-a-definition-list-with-pre-post-trimming}

The content in the `inline` shortcode (created for this test site)
should render _inline_ and not create a parapraph break at the end of
it.

defn1
: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque et
    quam metus. Etiam in iaculis mi, sit amet pretium magna. Donec ut
    dui mi. Maecenas pharetra sapien nunc, ut mollis enim aliquam
    quis.
{{% inline %}}
    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque et
    quam metus. Etiam in iaculis mi, sit amet pretium magna. Donec ut
    dui mi. Maecenas pharetra sapien nunc, ut mollis enim aliquam
    quis.
    {{% /inline %}} Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque et
    quam metus. Etiam in iaculis mi, sit amet pretium magna. Donec ut
    dui mi. Maecenas pharetra sapien nunc, ut mollis enim aliquam
    quis.

defn2
: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque et
    quam metus. Etiam in iaculis mi, sit amet pretium magna. Donec ut
    dui mi. Maecenas pharetra sapien nunc, ut mollis enim aliquam
    quis.


## Last element of a post {#last-element-of-a-post}
<mark>No "post" trim markers should be inserted after this block as it's the
last element of this post.</mark>
