+++
title = "Parsing notes from LOGBOOK"
description = """
  Parse notes from LOGBOOK into a TOML table (YAML map?) of `notes`
  front-matter.
  """
tags = ["front-matter", "notes", "logbook"]
draft = false
+++

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2018-09-06 Thu 11:45] </span></span> <br />
    Another note **bold** _italics_.
-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2018-09-06 Thu 11:37] </span></span> <br />
    A note `mono`.
-   State "DONE"       from "DRAFT"      <span class="timestamp-wrapper"><span class="timestamp">[2018-09-06 Thu 11:25]</span></span>
-   State "DRAFT"      from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2018-09-06 Thu 11:25]</span></span>
-   State "TODO"       from              <span class="timestamp-wrapper"><span class="timestamp">[2018-09-06 Thu 11:25]</span></span>

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

should export these `notes` in TOML front-matter (and the equivalent
in YAML):

```toml
[[notes]]
  index = 0
  date = 2018-09-06T11:37:00-04:00
  content = "A note."
[[notes]]
  index = 1
  date = 2018-09-06T11:45:00-04:00
  content = "Another note."
```

Note
: The state change notes are intentionally put in this test
    LOGBOOK, because we want to ensure that they don't seep into
    the `notes` front-matter.
