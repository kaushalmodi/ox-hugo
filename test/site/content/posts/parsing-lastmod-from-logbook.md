+++
title = "Parsing lastmod from LOGBOOK"
description = """
  If LOGBOOK has multiple entries of `"- State "DONE" from .."`, use the
  newest entry to parse the `lastmod` date from.
  """
tags = ["dates", "lastmod", "logbook"]
draft = false
[[org-logbook]]
  timestamp = 2018-09-06T11:41:00-04:00
  to-state = "DONE"
  from-state = "DRAFT"
[[org-logbook]]
  timestamp = 2018-09-06T11:40:00-04:00
  to-state = "DONE"
  from-state = "DRAFT"
+++

`ox-hugo` Issue #[203](https://github.com/kaushalmodi/ox-hugo/issues/203)

In the case of multiple `"State "DONE" from .."` entries,

-   Use the newest entry to set the `lastmod`.
-   Use the oldest entry to set the `date`.

\*Do not auto-set `lastmod` using `current-time` in this case.~
