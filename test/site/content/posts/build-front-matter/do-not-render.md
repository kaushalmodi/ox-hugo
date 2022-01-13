+++
title = "_build: Don't render this page"
description = "Do not render this page, but.. still list it."
tags = ["front-matter", "page-bundles", "_build", "dont-render"]
draft = false
[_build]
  render = false
+++

**This is the "do not render" page.**

See the [parent](../) list page -- This page is not rendered, but its content
and metadata can be found using the `site.GetPage` method. This page
also listed on that parent list page, but the link won't lead to this
page as.. it did not get rendered.

So, with the `_build.render` set to `false`, this page kind of behaves
like a [Page Resource](https://gohugo.io/content-management/page-resources) of "page" _ResourceType_. So we can still access
this page's `.Content` and other metadata like `.Title`, etc.
