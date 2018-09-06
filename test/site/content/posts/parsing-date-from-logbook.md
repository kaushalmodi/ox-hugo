+++
title = "Parsing date from LOGBOOK"
description = """
  Parse the date from an entry like `"- State "DONE" from "DRAFT"
  [2018-09-06 Thu 11:25]`.
  """
tags = ["dates", "date", "logbook"]
draft = false
+++

-   State "DONE"       from "TEST\__TODO" <span class="timestamp-wrapper"><span class="timestamp">[2022-01-11 Tue 11:22]</span></span>
-   State "DONE"       from "DRAFT"      <span class="timestamp-wrapper"><span class="timestamp">[2018-09-06 Thu 11:42]</span></span>
-   State "DONE"       from "DRAFT"      <span class="timestamp-wrapper"><span class="timestamp">[2018-09-06 Thu 11:38]</span></span>

`ox-hugo` Issue #[203](https://github.com/kaushalmodi/ox-hugo/issues/203)

In the case of multiple `"State "DONE" from .."` entries, use the
oldest entry to set the `date`.
