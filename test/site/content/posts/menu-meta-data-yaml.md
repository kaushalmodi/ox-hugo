---
title: "Menu Meta Data in YAML Front Matter"
date: 2017-07-18
tags: ["menu", "yaml"]
draft: false
menu:
  main:
    identifier: "yaml-menu-override"
    weight: 25
    parent: "posts"
---

Testing the addition of _menu_ meta data to the YAML front
matter. Here the front matter format is set to YAML using the
`#+hugo_front_matter_format` keyword.

Later only the _weight_ and _identifier_ menu properties are
overridden using `EXPORT_HUGO_MENU_OVERRIDE` key in the property drawer.
