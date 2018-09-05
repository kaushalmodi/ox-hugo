+++
title = "suppress lastmod with auto-set-lastmod"
author = ["Takaaki ISHIKAWA"]
date = 2118-09-01T08:35:00+00:00
draft = false
+++

This post will never export `lastmod` when you initially change the
Org TODO state to `DONE` by saving the file because
`org-hugo-suppress-lastmod-period` is always greater than the time
difference between `date` and `lastmod` in the following condition.

| Variable                         | Value |
|----------------------------------|-------|
| org-hugo-suppress-lastmod-period | 60.0  |
| org-hugo-auto-set-lastmod        | t     |
| org-hugo-auto-export-on-save     | t     |
| org-log-done                     | time  |

For instance, auto generated `date` would be `2018-09-01T08:00:00+00:00`
and `lastmod` could be `2018-09-01T08:00:59+00:00`. The time
difference is less than `org-hugo-suppress-lastmod-period` so
`lastmod` filed will not be exported. But if you change something in
this post after the initial exporting, the `lastmod` will be exported
because the time difference will exceed
`org-hugo-suppress-lastmod-period`.
