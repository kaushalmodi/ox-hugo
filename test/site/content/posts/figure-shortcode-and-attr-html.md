+++
title = "Image with Hugo figure shortcode parameters set using ATTR_HTML"
tags = ["image"]
draft = false
+++

[Issue # 17](https://github.com/kaushalmodi/ox-hugo/issues/17)


## Setting `class` parameter {#setting-class-parameter}

{{<figure src="/images/org-mode-unicorn-logo.png" class="inset">}}

[Discussion](https://github.com/kaushalmodi/ox-hugo/issues/17#issuecomment-313627728)


## Setting `alt` parameter {#setting-alt-parameter}

[Reference](https://www.reddit.com/r/emacs/comments/71wy6n/orgmode_as_a_markup_language_does_make_sense_even/dnhqudn/)

{{<figure src="/images/org-mode-unicorn-logo.png" alt="Org-mode Unicorn Logo">}}


## Setting `title` parameter {#setting-title-parameter}

{{<figure src="/images/org-mode-unicorn-logo.png" title="Logo">}}


## Setting image caption {#setting-image-caption}

The image caption can be set in two ways.

1.  Using the Org `#+CAPTION` keyword
2.  Using `#+ATTR_HTML: :caption my caption`

The `#+CAPTION` is available will get the higher precedence. In the
below image, caption is set using that:

Some text before image.

{{<figure src="/images/org-mode-unicorn-logo.png" caption="A unicorn!">}}
Some more text, after image.

Below, the same caption is set using the `#+ATTR_HTML` method instead:

Some text before image.

{{<figure src="/images/org-mode-unicorn-logo.png" caption="A unicorn!">}}
Some more text, after image.


## Other {#other}

Similarly, `:link`, `:attr`, `:attrlink` parameters in `#+ATTR_HTML`
are also supported to set the corresponding parameter in the Hugo
`figure` shortcode.
