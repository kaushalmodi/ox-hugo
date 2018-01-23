+++
title = "Invalid Date"
tags = ["dates", "date", "invalid"]
draft = false
+++

It's possible that someone is using an existing Org file to export to
Hugo. Some exporters like `ox-texinfo` recognize dates of style
`YEAR1-YEAR2` to use them in Copyright headers.

But that date is invalid as per the standard date format used by Hugo
in `date` front-matter, and also as per `org-parse-time-string`.

So in that case, don't allow `org-parse-time-string` to throw an error
and abort the export, but instead simply don't set the `date` in the
front-matter.

In this post the `:EXPORT_DATE:` property is set to `2012-2017`, but
the export will still happen fine, with the `date` front-matter not
set.
