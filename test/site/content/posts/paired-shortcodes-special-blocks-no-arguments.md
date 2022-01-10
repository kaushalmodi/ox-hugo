+++
title = "Paired Shortcodes using special blocks (No Arguments)"
description = "Tests for paired shortcodes with no arguments."
tags = ["shortcode", "paired", "special-block", "no-arguments"]
draft = false
+++

Test case for feature requested in `ox-hugo` Issue #[119](https://github.com/kaushalmodi/ox-hugo/issues/119).


## Paired markdown shortcode {#paired-markdown-shortcode}

```org
#+begin_mdshortcode
Content *with* /emphasis/ characters is rendered.
#+end_mdshortcode
```

Above will export as:

```md
{{%/* mdshortcode */%}}
Content *with* /emphasis/ characters is rendered.
{{%/* /mdshortcode */%}}
```

and render as:

{{% mdshortcode %}}
Content **with** _emphasis_ characters is rendered.
{{% /mdshortcode %}}


## Paired non-markdown (default) shortcode {#paired-non-markdown--default--shortcode}

Markdown is !! NOT !! rendered in non-markdown shortcodes.

```org
#+begin_myshortcode
The Markdown /emphasis/ characters are !! NOT !! rendered.

So a blank line before this sentence in the Markdown source will
not result in a new paragraph in HTML.
#+end_myshortcode
```

Above will export as:

```md
{{</* myshortcode */>}}
The Markdown /emphasis/ characters are !! NOT !! rendered.

So a blank line before this sentence in the Markdown source will
not result in a new paragraph in HTML.
{{</* /myshortcode */>}}
```

and render as:

{{< myshortcode >}}
The Markdown _emphasis_ characters are !! NOT !! rendered.

So a blank line before this sentence in the Markdown source will
not result in a new paragraph in HTML.
{{< /myshortcode >}}


## Not a recognized paired shortcode {#not-a-recognized-paired-shortcode}

<div class="foo">

Content **with** Markdown _emphasis_ characters is rendered fine in the
default Special Blocks.

</div>

This will export as [Special Blocks](/posts/special-blocks) --- either wrapped with `<div>`
tags or HTML5-recognized tags.
