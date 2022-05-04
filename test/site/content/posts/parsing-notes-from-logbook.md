+++
title = "Parsing notes from LOGBOOK"
description = """
  Parse notes from LOGBOOK into a TOML table (YAML map?) of `notes`
  front-matter.
  """
tags = ["front-matter", "notes", "logbook"]
draft = false
[[org-logbook]]
  timestamp = 2022-04-08T14:53:00-04:00
  note = """
    This note addition prompt shows up on typing the `C-c C-z` binding.
    See [Org Info: Drawers](https://orgmode.org/manual/Drawers.html "Emacs Lisp: (info \\"(org) Drawers\\")").
    """
[[org-logbook]]
  timestamp = 2018-09-06T11:45:00-04:00
  note = "Another note **bold** _italics_."
[[org-logbook]]
  timestamp = 2018-09-06T11:37:00-04:00
  note = "A note `mono`."
[[org-logbook]]
  timestamp = 2018-09-06T11:25:00-04:00
  to-state = "DONE"
  from-state = "DRAFT"
[[org-logbook]]
  timestamp = 2018-09-06T11:25:00-04:00
  to-state = "DRAFT"
  from-state = "TODO"
[[org-logbook]]
  timestamp = 2018-09-06T11:25:00-04:00
  to-state = "TODO"
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

should export these `notes` in arrays of TOML tables.

Note
: The state change notes are intentionally put in this test
    LOGBOOK, because we want to ensure that they don't seep into
    the `org-logbook` front-matter.
