+++
title = "LOGBOOK Notes in nested headings"
description = "Parse notes from LOGBOOK drawers in top-level and nested headings."
layout = "alternate-single"
tags = ["front-matter", "notes", "logbook"]
draft = false
[logbook]
  [logbook."Sub heading with markup"]
    [[logbook."Sub heading with markup".notes]]
      timestamp = 2022-05-11T17:12:00+00:00
      note = "Another note in a different sub heading."
  [logbook.Sub-heading]
    [[logbook.Sub-heading.notes]]
      timestamp = 2022-05-11T12:18:00+00:00
      note = "LOGBOOK note in a sub-heading"
  [logbook._toplevel]
    [[logbook._toplevel.notes]]
      timestamp = 2022-05-11T12:17:00+00:00
      note = "Note in the top-heading LOGBOOK drawer"
+++

The `lastmod` field is not set for this post (even if it has a note
with timestamp) because this post is not marked as DONE.


## Sub-heading {#sub-heading}


## Sub heading **with** _markup_ {#sub-heading-with-markup}
