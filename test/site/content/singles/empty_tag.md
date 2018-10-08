+++
title = "Post with an empty tag"
description = "Ensure that empty tag in Org file gets ignored."
tags = ["empty-tag", "tag"]
draft = false
+++

[`ox-hugo` issue #221](https://github.com/kaushalmodi/ox-hugo/issues/221)

The Org source of this post has:

```org
#+hugo_tags: "" "empty-tag"
```

This test ensures that the `""` tag gets ignored.
