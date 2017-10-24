+++
title = "Parsing date from CLOSED property"
date = 2017-09-11T14:32:00+00:00
tags = ["post-heading-followed-soon-with-subheading"]
draft = false
+++

## The "CLOSED" state of this heading (which is nil) should be ignored {#the-closed-state-of-this-heading--which-is-nil--should-be-ignored}

When an Org TODO item is switched to the `DONE` state, a `CLOSED`
property is auto-inserted (default behavior).

If such a property is non-nil, the value (time-stamp) of that is used
to set the `date` field in the exported front-matter.

-   **Reference:** [(org) Special properties](http://orgmode.org/manual/Special-properties.html) or `C-h i g (org) Special properties`
