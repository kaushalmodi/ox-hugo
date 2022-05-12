+++
title = "LOGBOOK Notes in nested headings"
description = "Parse notes from LOGBOOK drawers in top-level and nested headings."
layout = "alternate-single"
lastmod = 2022-05-11T12:17:00+00:00
tags = ["front-matter", "notes", "logbook"]
draft = false
[logbook]
  [logbook."Sub heading with markup"]
    [[logbook."Sub heading with markup".notes]]
      timestamp = 2022-05-11T17:12:00+00:00
      note = "Another note in a different sub heading."
    [[logbook."Sub heading with markup".notes]]
      timestamp = 2022-05-12T13:44:00+00:00
      note = """
  Testing a pathogenic case where the user has forcefully inserted a
  `:LOGBOOK:` drawer in the middle of subtree content.
  """
  [logbook.Sub-heading]
    [[logbook.Sub-heading.notes]]
      timestamp = 2022-05-11T12:18:00+00:00
      note = "LOGBOOK note in a sub-heading"
  [logbook._toplevel]
    [[logbook._toplevel.notes]]
      timestamp = 2022-05-11T12:17:00+00:00
      note = "Note in the top-heading LOGBOOK drawer"
+++

## Sub-heading {#sub-heading}


## Sub heading **with** _markup_ {#sub-heading-with-markup}


Some text in body between two `:LOGBOOK:` drawers.


Text after the second `:LOGBOOK:` drawer.
