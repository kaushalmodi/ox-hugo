+++
title = "Links with target attribute"
tags = ["links"]
draft = false
+++

<a href="https://orgmode.org/manual/Hyperlinks.html" target="_blank" rel="noopener">This link (to Hyperlinks chapter in Org manual)</a> will open in a new tab
as it is annotated with `target="_blank"`.

<a href="https://orgmode.org/manual/Hyperlinks.html" target="_self">Here's the same link</a> but with `target="_self"` annotation. So
clicking it will open that link in this same tab!

[Here's the same link again](https://orgmode.org/manual/Hyperlinks.html), but this time there is no `#+attr_html`
annotation. So the behavior of clicking this link will depend on the
browser (typically an external link will open in a new tab
automatically).


## Image linked to image with `target` attribute {#image-linked-to-image-with-target-attribute}

`ox-hugo` Issue #[133](https://github.com/kaushalmodi/ox-hugo/issues/133)

{{< figure src="https://ox-hugo.scripter.co/test/images/org-mode-unicorn-logo.png" width="10%" target="_self" link="https://ox-hugo.scripter.co/test/images/org-mode-unicorn-logo.png" >}}

Above is an hyperlinked image with the HTML attributes set as
`#+attr_html: :width 10% :target _self` in Org.

-   The `width` attribute of _10%_ must apply **only** to the image, and
    not to the link.
-   And the `target="_self"` attribute must apply **only** to the link,
    and not the image. So, clicking that image will open the linked
    image in the same browser tab because the `target` is set to
    `"_self"` (**Hugo v0.37+** --- fixed in `hugo` PR #[4382](https://github.com/gohugoio/hugo/pull/4382)).

---

Here's an inline hyperlinked image with the exact same HTML
attributes: <a href="https://ox-hugo.scripter.co/test/images/org-mode-unicorn-logo.png" target="_self"><img src="https://ox-hugo.scripter.co/test/images/org-mode-unicorn-logo.png" alt="org-mode-unicorn-logo.png" width="10%" /></a>. So,
clicking this image too should open the linked image in the same
brower tab.
