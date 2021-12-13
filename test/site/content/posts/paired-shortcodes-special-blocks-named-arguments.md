+++
title = "Paired shortcodes using special blocks (Named Arguments)"
description = "Tests for paired shortcodes with named arguments."
tags = ["shortcode", "paired", "special-block", "arguments", "named"]
draft = false
+++

Test case for feature requested in `ox-hugo` Issue #[119](https://github.com/kaushalmodi/ox-hugo/issues/119).


## Paired markdown shortcode {#paired-markdown-shortcode}

In the below example, `"foo bar"` will be the _arg1_ argument, and
`"color: red; text-align: center;"` will be the _arg2_ argument.

{{% mdshortcode-named arg1="foo bar" arg2="color: red; text-align: center;" %}}
Content **with** _emphasis_ characters is rendered.
{{% /mdshortcode-named %}}


## Paired non-markdown (default) shortcode {#paired-non-markdown--default--shortcode}

In the below example, `"foo"` will be the _arg1_ argument, and
`"color:green;"` will be the _arg2_ argument.

{{< myshortcode-named arg1="foo" arg2="color:green;" >}}
The Markdown _emphasis_ characters are !! NOT !! rendered.
{{< /myshortcode-named >}}
