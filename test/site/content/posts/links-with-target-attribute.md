+++
title = "Links with target attribute"
tags = ["links"]
draft = false
[blackfriday]
  hrefTargetBlank = false
+++

<a href="http://orgmode.org/manual/Hyperlinks.html" target="_blank" rel="noopener">This link (to Hyperlinks chapter in Org manual)</a> will open in a new tab
as it is annotated with `target="_blank"`.

<a href="http://orgmode.org/manual/Hyperlinks.html" target="_self">Here's the same link</a> but with `target="_self"` annotation. So
clicking it will open that link in this same tab!

[Here's the same link again](http://orgmode.org/manual/Hyperlinks.html), but this time there is no `#+ATTR_HTML`
annotation. So the behavior will depend on the browser (typically an
external link will open in a new tab automatically).
