+++
title = "Hugo auto weight ineffective for per-file exports"
tags = ["weight", "ineffective"]
draft = false
+++

Even though we have `#+hugo_weight: auto` in this file, the weight
will be treated as nil as this file is exported using the _file-based_
export.

The auto-weight calculation works **only** for _per-subtree_ flow.
