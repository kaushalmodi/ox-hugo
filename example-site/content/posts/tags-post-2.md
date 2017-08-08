+++
title = "Specifying additional tags"
tags = ["gamma", "delta", "alpha", "beta"]
draft = false
+++

If user specifies `EXPORT_HUGO_TAGS` in the property drawer, those
tags get added to the set of default tags set in `#+TAGS` (if
any).  These tags are collected together and assigned to the Hugo
`tags` front matter variable.
