+++
title = "Writing Hugo blog in Org (File Export)"
author = ["Kaushal Modi"]
date = 2017-09-10
lastmod = 2100-12-21T00:00:00+00:00
tags = ["hugo", "org"]
categories = ["emacs"]
draft = true
weight = 2001
foo = "bar"
baz = "zoo"
alpha = 1
beta = "two words"
gamma = 10
[menu.main]
  identifier = "writing-hugo-blog-in-org-file-export"
  weight = 2001
+++

## First heading within the post {#first-heading-within-the-post}

-   This post will be exported as
    `content/posts/writing-hugo-blog-in-org-file-export.md`.
-   Its title will be "Writing Hugo blog in Org".
-   It will have _hugo_ and _org_ tags and _emacs_ as category.
-   The _lastmod_ property in the front-matter is set automatically to
    the time of export.
-   The menu item _identifier_ is auto-set.
-   The menu item _weight_ and post _weight_ if needed have to be
    manually specified as shown above.


### A sub-heading under that heading {#a-sub-heading-under-that-heading}

-   It's draft state will be marked as `true` because of `#+hugo_draft:
      true`.

With the point <span class="underline">anywhere</span>, do `C-c C-e H h` to export this whole file
titled _Writing Hugo blog in Org_ to a Hugo post.

The exported Markdown has a little comment footer as set in the _Local
Variables_ section below.

[//]: # "Exported with love from a post written in Org mode"
[//]: # "- https://github.com/kaushalmodi/ox-hugo"
