+++
title = "Overriding Org-style tags"
tags = ["overriding", "underscore_is_retained", "hyphenated-works"]
categories = ["cat3", "3 word cat"]
draft = false
+++

By using `EXPORT_HUGO_TAGS` in the property drawer, Org tags in the
current heading ("this_tag_wont_apply") **and** the inherited one
("alpha", "beta", "hyphenated-tag", "super") will get overridden.

When setting categories via the keyword `#+hugo_categories` or the
subtree property `EXPORT_HUGO_CATEGORIES`, do **not** add the "@" prefix.
