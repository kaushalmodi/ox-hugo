+++
title = "Unsuppress lastmod"
description = "`lastmod` front-matter parameter is **not** suppressed in this post."
date = 2100-12-10
lastmod = 2100-12-21T00:00:00+00:00
tags = ["suppress", "lastmod", "nonzero", "autoset"]
draft = false
+++

`org-hugo-suppress-lastmod-period` is set to **172800.0** seconds (2
days) in Local Variables.

_For tests, the "current date" is always Dec 21, 2100._

The export date is set to be 11 days prior to the "current date". This
is more than the suppress period of 2 days. So this post will export
`lastmod`.
