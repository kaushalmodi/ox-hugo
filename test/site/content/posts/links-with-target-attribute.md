+++
title = "Links with target attribute"
tags = ["links"]
draft = false
[blackfriday]
  hrefTargetBlank = false
+++

<a href="https://orgmode.org/manual/Hyperlinks.html" target="_blank" rel="noopener">This link (to Hyperlinks chapter in Org manual)</a> will open in a new tab
as it is annotated with `target="_blank"`.

<a href="https://orgmode.org/manual/Hyperlinks.html" target="_self">Here's the same link</a> but with `target="_self"` annotation. So
clicking it will open that link in this same tab!

<a href="https://orgmode.org/img/org-mode-unicorn-logo.png" target="_self">{{<figure src="https://orgmode.org/img/org-mode-unicorn-logo.png" width="10%">}}</a>

Above is a link to an image. The `width` attribute of _10%_ though
must apply **only** to the image, and not to the link, and the `target`
attribute must apply **only** to the link, and not to the image.

[Here's the same link again](https://orgmode.org/manual/Hyperlinks.html), but this time there is no `#+ATTR_HTML`
annotation. So the behavior will depend on the browser (typically an
external link will open in a new tab automatically).
