+++
title = "Parsing date from LOGBOOK"
description = """
  Parse the date from an entry like `"- State "DONE" from "DRAFT"
  [2018-09-06 Thu 11:25]`.
  """
tags = ["dates", "date", "logbook"]
draft = false
+++

`ox-hugo` Issue #[203](https://github.com/kaushalmodi/ox-hugo/issues/203)

In the case of multiple `"State "DONE" from .."` entries, use the
oldest entry to set the `date`.
