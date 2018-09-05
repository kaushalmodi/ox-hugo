+++
title = "Writing Hugo blog in Org"
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
  weight = 2001
  identifier = "writing-hugo-blog-in-org"
+++

## First heading within the post {#first-heading-within-the-post}

-   This post will be exported as
    `content/posts/writing-hugo-blog-in-org-subtree-export.md`.
-   Its title will be "Writing Hugo blog in Org".
-   It will have _hugo_ and _org_ tags and _emacs_ as category.
-   The menu item _weight_ and post _weight_ are auto-calculated.
-   The menu item _identifier_ is auto-set.
-   The _lastmod_ property in the front-matter is set automatically to
    the time of export.


### A sub-heading under that heading {#a-sub-heading-under-that-heading}

-   It's draft state will be marked as `true` as the subtree has the
    todo state set to _TODO_.

With the point <span class="underline">anywhere</span> in this _Writing Hugo blog in Org_ post
subtree, do `C-c C-e H H` to export just this post.

The exported Markdown has a little comment footer as set in the _Local
Variables_ section below.

[//]: # "Exported with love from a post written in Org mode"
[//]: # "- https://github.com/kaushalmodi/ox-hugo"
