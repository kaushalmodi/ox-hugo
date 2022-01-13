+++
title = "_build: Don't render this page"
description = "Do not render this page, but.. still list it."
tags = ["front-matter", "page-bundles", "_build", "dont-render"]
draft = false
[_build]
  render = "never"
+++

[`_build.render` option](https://gohugo.io/content-management/build-options/#render)

**This is the "do not render" page.**

See the [parent](../) list page -- This page is not rendered, but its content
and metadata can be found using the `site.GetPage` method.

<mark>This page is listed on that parent list page, but the link won't lead
to this page as.. it did not get rendered. That link will point to
that page itself because with `render` set to `false` or `"never"`,
the `.Permalink` and `.RelPermalink` get set to `""`.</mark>
