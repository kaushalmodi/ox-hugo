+++
title = "Writing Hugo blog in Org"
date = 2017-09-10
lastmod = 2017-09-10T01:49:05-04:00
tags = ["hugo", "org"]
categories = ["emacs"]
weight = 2001
draft = false
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

-   This post will be exported as `content/posts/writing-hugo-blog-in-org.md`.
-   Its title will be "Writing Hugo blog in Org".
-   It will have _hugo_ and _org_ tags and _emacs_ as category.
-   The menu item _weight_ and post _weight_ are auto-calculated.
-   The menu item _identifier_ is auto-set.
-   The _lastmod_ property in the front-matter is set automatically to
    the time of export.


### A sub-heading under that heading {#a-sub-heading-under-that-heading}

-   It's draft state will be marked as `false` as the subtree doesn't
    have the todo state set to _TODO_ or _DRAFT_.

[//]: # "Exported with love from a post written in Org mode"
[//]: # "- https://github.com/kaushalmodi/ox-hugo"
