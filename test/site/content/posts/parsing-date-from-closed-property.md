+++
title = "Parsing date from CLOSED property"
date = 2018-01-23T14:10:00+00:00
tags = ["dates", "date", "closed"]
draft = false
+++

When an Org TODO item is switched to the `DONE` state, a `CLOSED`
property is auto-inserted (default behavior).

If such a property is non-nil, the value (time-stamp) of that is used
to set the `date` field in the exported front-matter.

Reference
: [(org) Special properties](https://orgmode.org/manual/Special-properties.html) or `C-h i g (org) Special properties`
