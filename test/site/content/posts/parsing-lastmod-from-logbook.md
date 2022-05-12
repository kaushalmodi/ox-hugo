+++
title = "Parsing lastmod from LOGBOOK"
description = """
  If LOGBOOK has multiple entries of `"- State "DONE" from .."`, use the
  newest entry to parse the `lastmod` date from.
  """
date = 2018-09-06T11:40:00+00:00
lastmod = 2018-09-06T11:41:00+00:00
tags = ["dates", "lastmod", "logbook"]
draft = false
+++

`ox-hugo` Issue #[203](https://github.com/kaushalmodi/ox-hugo/issues/203)

In the case of multiple `"State "DONE" from .."` entries,

-   Use the newest entry to set the `lastmod`.
-   Use the oldest entry to set the `date`.

The `lastmod` field parsed from LOGBOOK transitions will have the
highest precedence. So it will **not be auto-set** even if the
`:EXPORT_HUGO_AUTO_SET_LASTMOD: t` property is set.
