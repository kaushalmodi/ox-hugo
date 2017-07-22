+++
title = "Overriding few menu properties"
date = 2017-07-18
tags = ["menu"]
draft = false
[menu."something here"]
  parent = "posts"
  weight = 10
  identifier = "ov-partial"
+++

For this post, we should see just the menu _weight_ and _identifier_
properties get overridden.

You need to set unique menu identifiers, else you get a Hugo error
like this:

```text
ERROR 2017/07/18 12:32:14 Two or more menu items have the same name/identifier in Menu "main": "menu-meta-data-in-yaml-front-matter".
Rename or set an unique identifier.
```
