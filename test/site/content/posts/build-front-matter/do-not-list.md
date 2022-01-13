+++
title = "_build: Don't list this page"
description = "Build this page but don't list it in _list_ pages."
tags = ["front-matter", "page-bundles", "_build", "dont-list"]
draft = false
[_build]
  list = "never"
+++

[`_build.list` option](https://gohugo.io/content-management/build-options/#list)

**This is the "do not list" page.**

See the [parent](../) list page -- This page is found using the `.GetPage`
method there, but it won't be listed there under "Posts in
‘posts/build-front-matter/’".
