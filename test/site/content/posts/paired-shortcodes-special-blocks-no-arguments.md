+++
title = "Paired Shortcodes using special blocks (No Arguments)"
tags = ["shortcode", "paired", "special-block", "no-arguments"]
draft = false
+++

Test case for feature requested in `ox-hugo` Issue #[119](https://github.com/kaushalmodi/ox-hugo/issues/119).


## Paired markdown shortcode {#paired-markdown-shortcode}

{{% mdshortcode %}}
Content **with** _emphasis_ characters is rendered.

The HTML <b>markup</b> will also get rendered.
{{% /mdshortcode %}}

This will export as:

```md
{{%/* mdshortcode */%}} Content rendered as Markdown {{%/* /mdshortcode */%}}
```


## Paired non-markdown (default) shortcode {#paired-non-markdown--default--shortcode}

{{< myshortcode >}}
Content is rendered <b>like HTML</b>. The Markdown _emphasis_
characters are !! NOT !! rendered.

So a blank line before this sentence in the Markdown source will
<b>not</b> result in a new paragraph in HTML. <p>But this will be a
new paragraph as it is wrapped in HTML <code>&lt;p&gt;</code>
tags.</p>
{{< /myshortcode >}}

This will export as:

```md
{{</* myshortcode */>}} Content NOT rendered as Markdown {{</* /myshortcode */>}}
```


## Not a recognized paired shortcode {#not-a-recognized-paired-shortcode}

<div class="foo">
  <div></div>

Content **with** Markdown _emphasis_ characters is rendered fine in the
default Special Blocks.

</div>

This will export as [Special Blocks](/posts/special-blocks) --- either wrapped with `<div>`
tags or HTML5-recognized tags.
