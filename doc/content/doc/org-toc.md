+++
title = "Table of Contents"
draft = false
[menu."org_to_hugo"]
  weight = 3006
  identifier = "table-of-contents"
+++

Hugo can automatically parse the Markdown content and auto-create a
Table of Contents. See its documentation on [Table of Contents](https://gohugo.io/content-management/toc/). So
`ox-hugo` doesn't generate the Org-parsed TOC by default.

The advantage of using Hugo-generated TOC is that it does not clutter
the Markdown source.

Though, the advantage of Org-generated TOC is that you get finer
control on:

-   Where to include the TOC --- Location of the `#+TOC` keyword in the
    Org content.
-   How many headlines to include in the TOC --- _Example: `#+TOC:
      headlines 2`_ or `:EXPORT_OPTIONS: toc:2`.
-   Whether you want to **not** include a headline in the TOC --- Set the
    `UNNUMBERED` property of that headline to `t`.

If you'd like to use the Org-generated TOC instead of the
Hugo-generated one, you can do it one of these many ways:

1.  The default is to use the Hugo-generated TOC. But that can be
    changed by setting `org-hugo-export-with-toc` variable to a non-nil
    value, like `t` or `2`.
2.  Org-generated TOC can be enabled per-post by either setting
    `EXPORT_OPTIONS` subtree property (for subtree-based exports) or
    the `OPTIONS` keyword (for file-based exports) to a non-nil value,
    like `toc:t` or `toc:2`.
3.  Above two options will insert the TOC between the front-matter and
    the Markdown content. If you'd like to insert the Org-generated TOC
    anywhere else in the post, you can do it using the `#+TOC`
    keyword.. Example: `#+TOC: headlines 2`.

See Org manual [Table of Contents](http://orgmode.org/manual/Table-of-contents.html) section for more info.

_Note that `ox-hugo` does not support `#+TOC: listings` and `#+TOC:
tables`._
