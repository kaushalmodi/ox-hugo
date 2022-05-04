+++
title = "Parsing date from LOGBOOK"
description = """
  Parse the date from an entry like `"- State "DONE" from "DRAFT"
  [2018-09-06 Thu 11:25]`.
  """
tags = ["dates", "date", "logbook"]
draft = false
[[org-logbook]]
  timestamp = 2022-01-11T11:22:00-05:00
  to-state = "DONE"
  from-state = "TEST\\__TODO"
[[org-logbook]]
  timestamp = 2018-09-06T11:42:00-04:00
  to-state = "DONE"
  from-state = "DRAFT"
[[org-logbook]]
  timestamp = 2018-09-06T11:38:00-04:00
  to-state = "DONE"
  from-state = "DRAFT"
+++

`ox-hugo` Issue #[203](https://github.com/kaushalmodi/ox-hugo/issues/203)

In the case of multiple `"State "DONE" from .."` entries, use the
oldest entry to set the `date`.
