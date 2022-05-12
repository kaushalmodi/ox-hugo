+++
title = "Post marked as DONE with LOGBOOK Notes in nested headings"
description = "Parse notes from LOGBOOK drawers in top-level and nested headings."
date = 2022-05-01T08:49:00+00:00
layout = "alternate-single"
tags = ["front-matter", "notes", "logbook", "lastmod"]
draft = false
[logbook]
  [logbook.Sub-heading]
    [[logbook.Sub-heading.notes]]
      timestamp = 2022-05-12T08:49:00+00:00
      note = """
  This LOGBOOK note was added in a sub-heading **after** the post was
  marked DONE, but this note's timestamp should not affect the post's
  `lastmod` field (because this is not the toplevel LOGBOOK drawer).
  """
+++

The `lastmod` field is not set for this post (even if it has a note
with timestamp) because this post is not marked as DONE.


## Sub-heading {#sub-heading}
