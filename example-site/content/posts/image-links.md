+++
title = "Image links"
date = 2017-07-15T07:49:44-04:00
tags = ["image"]
draft = false
+++

This is some text before the first heading of this post.


## Unclickable image (works!) {#unclickable-image--works}

{{<figure src="/images/org-mode-unicorn-logo.png">}}

---

~~To be fixed~~ (**Now fixed**): The sub-headings in a post get exported as _Heading 1_
 instead of _Heading 2_.

For example, this sub-section&rsquo;s heading is exported as:

```text
# Unclickable image
```

instead of

```text
## Unclickable image
```

---

**Solution**: Above is fixed by setting `HUGO_OFFSET_LEVEL` to 1.

So the sub-heading title and the post title both get the _Heading 1_
tag and look the same size.


## Clickable link that opens the image (works!) {#clickable-link-that-opens-the-image--works}

[Click here to see the unicorn](/images/org-mode-unicorn-logo.png)


## Clickable image that opens the image (works!) {#clickable-image-that-opens-the-image--works}

Click below image to jump to the unicorn image.
[{{<figure src="/images/org-mode-unicorn-logo.png">}}](/images/org-mode-unicorn-logo.png)

-   **NOTE:** `file:` has to be used in both Link and Description components
    of the Org link.


## Image with `ATTR_HTML` [Issue # 17](https://github.com/kaushalmodi/ox-hugo/issues/17) {#image-with-attr-html-issue-17}

{{<figure src="/images/org-mode-unicorn-logo.png" class="inset">}}

[Discussion](https://github.com/kaushalmodi/ox-hugo/issues/17#issuecomment-313627728)


## Link to image outside of standard Hugo locations {#link-to-image-outside-of-standard-hugo-locations}

{{<figure src="/images/copy-of-unicorn-logo.png">}}
