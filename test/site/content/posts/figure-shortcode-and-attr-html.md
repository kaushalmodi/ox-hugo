+++
title = "Image with Hugo figure shortcode parameters set using ATTR_HTML"
description = "Figure captions with markup."
tags = ["image"]
draft = false
+++

`ox-hugo` Issue #[17](https://github.com/kaushalmodi/ox-hugo/issues/17)


## Setting `class` parameter {#setting-class-parameter}

{{< figure src="/images/org-mode-unicorn-logo.png" class="inset" >}}

[Discussion](https://github.com/kaushalmodi/ox-hugo/issues/17#issuecomment-313627728)


## Setting `alt` parameter {#setting-alt-parameter}

[Reference](https://www.reddit.com/r/emacs/comments/71wy6n/orgmode_as_a_markup_language_does_make_sense_even/dnhqudn/)

{{< figure src="/images/org-mode-unicorn-logo.png" alt="Org-mode Unicorn Logo" >}}


## Setting `title` parameter {#setting-title-parameter}

{{< figure src="/images/org-mode-unicorn-logo.png" title="Logo" >}}


## Setting image caption {#setting-image-caption}

The image caption can be set in two ways.

1.  Using the Org `#+caption` keyword
2.  Using `#+attr_html: :caption my caption`

The `#+caption` is available will get the higher precedence. In the
below image, caption is set using that:

Some text before image.

{{< figure src="/images/org-mode-unicorn-logo.png" caption="<span class=\"figure-number\">Figure 1: </span>A unicorn! \"Something in double quotes\"" >}}

Some more text, after image.

{{< figure src="/images/org-mode-unicorn-logo.png" caption="<span class=\"figure-number\">Figure 2: </span>The _same_ figure **again**, testing [a link](https://ox-hugo.scripter.co) too!" >}}

Below, the same caption is set using the `#+attr_html` method instead:

Some text before image.

{{< figure src="/images/org-mode-unicorn-logo.png" caption="<span class=\"figure-number\">Figure 3: </span>A unicorn!" >}}

_Enter a new line after the image link so that it's in an "Org
paragraph" that contains just that image. That tells Org that that
`#+attr_html` attribute is associated **only** with that image, and not
to the text that follows that image too._


## Setting image size {#setting-image-size}


### Setting `:width` parameter {#setting-width-parameter}

The image [width](https://www.w3schools.com/tags/att_img_width.asp) can be specified in **pixels** using the `:width`
parameter. **The height of the image will be resized proportionally.**

Below image is shown 50 pixels wide.

{{< figure src="/images/org-mode-unicorn-logo.png" width="50" >}}

Below image is shown 100 pixels wide.

{{< figure src="/images/org-mode-unicorn-logo.png" width="100" >}}

Below image is shown with a width of 1000 pixels! Now the size of this
image is 200px × 200px. But the image will still show up in 1000px ×
1000px size, but obviously heavily pixelated!

{{< figure src="/images/org-mode-unicorn-logo.png" width="1000" >}}


### Setting `:height` parameter {#setting-height-parameter}

**NOTE**: Support for specifying `height` parameter to the Hugo `figure`
 shortcut was only added recently in [hugo PR #4018](https://github.com/gohugoio/hugo/pull/4018). So setting this
 parameter will need **hugo v0.31** or later.

---

The image [height](https://www.w3schools.com/tags/att_img_height.asp) can be specified in **pixels** using the `:height`
parameter. **The weight of the image will be resized proportionally.**

Below image is shown 50 pixels tall.

{{< figure src="/images/org-mode-unicorn-logo.png" height="50" >}}

Below image is shown 100 pixels tall.

{{< figure src="/images/org-mode-unicorn-logo.png" height="100" >}}

Below image is shown with a height of 1000 pixels! Now the size of
this image is 200px × 200px. But the image will still show up in
1000px × 1000px size, but obviously heavily pixelated!

{{< figure src="/images/org-mode-unicorn-logo.png" height="1000" >}}


### Setting both `:width` and `:height` {#setting-both-width-and-height}

The **NOTE** above applies here too.. needs **hugo v0.31** or later.

The figure sizes below are intentionally set _mis-proportionally_ just
for testing.

-   `:width 100 :height 200`

    {{< figure src="/images/org-mode-unicorn-logo.png" width="100" height="200" >}}
-   `:width 200 :height 100`

    {{< figure src="/images/org-mode-unicorn-logo.png" width="200" height="100" >}}


## Multiple `ATTR_HTML` {#multiple-attr-html}

Below in Org source:

```org
#+html: <style>.foo img { border:2px solid black; }</style>
#+attr_html: :alt Org mode logo
#+attr_html: :width 300 :class foo
[[https://ox-hugo.scripter.co/test/images/org-mode-unicorn-logo.png]]
```

Rendered this:

<style>.foo img { border:2px solid black; }</style>

{{< figure src="https://ox-hugo.scripter.co/test/images/org-mode-unicorn-logo.png" alt="Org mode logo" class="foo" width="300" >}}

**NOTE**: We cannot use `:style` in `#+attr_html` because Hugo does not
 _yet_ support a `style` argument in the `figure` shortcode [ [Source](https://github.com/gohugoio/hugo/blob/488631fe0abc3667355345c7eb98ba7a2204deb5/tpl/tplimpl/template_embedded.go#L22-L37)
 ].

So using `#+html: <style>.foo img ... </style>` and `#+attr_html: :class
 foo` as shown in the workaround above.


## Other {#other}

Similarly, `:link`, `:attr`, `:attrlink` parameters in `#+attr_html`
are also supported to set the corresponding parameter in the Hugo
`figure` shortcode.
