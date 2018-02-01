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

[Here's the same link again](https://orgmode.org/manual/Hyperlinks.html), but this time there is no `#+ATTR_HTML`
annotation. So the behavior of clicking this link will depend on the
browser (typically an external link will open in a new tab
automatically).


## Image linked to image with `target` attribute {#image-linked-to-image-with-target-attribute}

{{< figure src="https://orgmode.org/img/org-mode-unicorn-logo.png" width="10%" link="https://orgmode.org/img/org-mode-unicorn-logo.png" >}}

Above is a link to an image. The `width` attribute of _10%_ though
must apply **only** to the image, and not to the link.

Note
: The `target` attribute is discarded though in this case
    because of the Hugo `figure` shortcode limitation.. there is
    no way to pass the `target` or any other attribute to the
    `<a>` element inside the `<figure>` element that the
    `figure` shortcode constructs.
