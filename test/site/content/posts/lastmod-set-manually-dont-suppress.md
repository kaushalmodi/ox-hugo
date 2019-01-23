+++
title = "Don't suppress lastmod as it is not set automatically"
description = """
  `lastmod` front-matter parameter is **not** suppressed in this post
  because its value is set **manually**.
  """
date = 2100-12-21
lastmod = 2100-12-21
tags = ["suppress", "lastmod", "nonzero", "manual"]
draft = false
+++

`org-hugo-suppress-lastmod-period` is set to **172800.0** seconds (2
days) in Local Variables.

_For tests, the "current date" is always Dec 21, 2100._

The value of `org-hugo-suppress-lastmod-period` is ignored in this
case as auto-setting of `lastmod` is disabled.
