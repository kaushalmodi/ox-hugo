+++
title = "Parsing notes from LOGBOOK"
description = """
  Parse notes from LOGBOOK into a TOML table (YAML map?) of
  `logbook.notes` front-matter.
  """
date = 2018-09-06T11:25:00+00:00
layout = "alternate-single"
lastmod = 2022-05-12T08:13:00+00:00
tags = ["front-matter", "notes", "logbook"]
draft = false
[logbook]
  [logbook._toplevel]
    [[logbook._toplevel.notes]]
      timestamp = 2022-05-12T08:13:00+00:00
      note = """
  Another update to the post. This update timestamp should update the
  `lastmod`.
  """
    [[logbook._toplevel.notes]]
      timestamp = 2022-05-04T13:15:00+00:00
      note = """
  This new note added last should be the first element of the
  `[[logbook.notes]]` TOML table array.
  """
    [[logbook._toplevel.notes]]
      timestamp = 2022-04-08T14:53:00+00:00
      note = """
  This note addition prompt shows up on typing the `C-c C-z` binding.
  See [Org Info: Drawers](https://orgmode.org/manual/Drawers.html "Emacs Lisp: (info \\"(org) Drawers\\")").
  """
    [[logbook._toplevel.notes]]
      timestamp = 2018-09-06T11:45:00+00:00
      note = "Another note **bold** _italics_."
    [[logbook._toplevel.notes]]
      timestamp = 2018-09-06T11:37:00+00:00
      note = "A note `mono`."
+++

`ox-hugo` Issue #[203](https://github.com/kaushalmodi/ox-hugo/issues/203)

For example,

```org
:LOGBOOK:
- Note taken on [2018-09-06 Thu 11:45] \\
  Another note.
- Note taken on [2018-09-06 Thu 11:37] \\
  A note
- State "DONE"       from "DRAFT"      [2018-09-06 Thu 11:25]
- State "DRAFT"      from "TODO"       [2018-09-06 Thu 11:25]
- State "TODO"       from              [2018-09-06 Thu 11:25]
:END:
```

should export only the notes to an array of TOML tables with key
`logbook.<..>.notes`. The notes are ordered starting from the newest
note first in the TOML table array to the oldest note at the last.

Note
: The state change notes are intentionally put in this test
    LOGBOOK, because we want to ensure that they don't seep into the
    `logbook.<..>.notes` front-matter.
