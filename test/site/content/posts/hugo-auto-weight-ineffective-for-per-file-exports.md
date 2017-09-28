+++
title = "Hugo auto weight ineffective for per-file exports"
tags = ["weight"]
draft = false
+++

Even though we have `#+HUGO_WEIGHT: auto` in this file, the weight
will be treated as nil as this file is exported using the _per-file_
or _complete-file_ export flow.

The auto-weight calculation works **only** for _per-subtree_ flow.
