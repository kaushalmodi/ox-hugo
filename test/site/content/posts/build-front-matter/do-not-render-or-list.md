+++
title = "_build: Don't render or list this page"
description = "Do not render or list this page."
tags = ["front-matter", "page-bundles", "_build", "dont-render", "dont-list", "headless"]
draft = false
[_build]
  render = false
  list = false
+++

**This is the "do not render or list" page.**

See the [parent](../) list page -- This page is not rendered, but its content
and metadata can be found using the `site.GetPage` method.

Setting both `render` and `list` to `false` or `"never"` makes this
page behave like a **headless** page bundle -- [ref](https://github.com/gohugoio/hugo/issues/6412#issuecomment-573446730).
