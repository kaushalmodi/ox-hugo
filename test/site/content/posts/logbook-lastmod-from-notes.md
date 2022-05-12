+++
title = "Parse lastmod from LOGBOOK Notes with no recorded state changes"
description = """
  Parse `lastmod` from toplevel LOGBOOK notes even if the drawer didn't
  record any state changes.
  """
date = 2022-05-10T00:00:00+00:00
layout = "alternate-single"
lastmod = 2022-05-12T10:56:00+00:00
tags = ["front-matter", "notes", "logbook", "lastmod"]
draft = false
[logbook]
  [logbook._toplevel]
    [[logbook._toplevel.notes]]
      timestamp = 2022-05-12T10:56:00+00:00
      note = "Some note."
+++
