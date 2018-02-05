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

[Here's the same link again](https://orgmode.org/manual/Hyperlinks.html), but this time there is no `#+attr_html`
annotation. So the behavior of clicking this link will depend on the
browser (typically an external link will open in a new tab
automatically).


## Image linked to image with `target` attribute {#image-linked-to-image-with-target-attribute}

`ox-hugo` Issue #[133](https://github.com/kaushalmodi/ox-hugo/issues/133)

{{< figure src="https://orgmode.org/img/org-mode-unicorn-logo.png" width="10%" target="_self" link="https://orgmode.org/img/org-mode-unicorn-logo.png" >}}

Above is a link to an image.

-   The `width` attribute of _10%_ though must apply **only** to the
    image, and not to the link.
-   Clicking that image will open the linked image in the same browser
    tab because of the `target="_self"` attribute (**because of the
    custom shortcode** --- see below).

<!--listend-->

Note 1
: **By default** (using the Hugo-inbuilt `figure` shortcode),
    the `target` attribute is _ineffective_. There is no way
    to pass the `target` or any other attribute to the `<a>`
    element inside the `<figure>` element that the `figure`
    shortcode constructs (commit [e92fcf00](https://github.com/kaushalmodi/ox-hugo/commit/e92fcf00)).

Note 2
: This test site uses a custom `figure` shortcode with
    support added for specifying the `target` and `rel`
    attributes. If you too like to retain hyperlinked figures'
    `target` and `rel` attributes, get the custom shortcode
    [from here](https://github.com/kaushalmodi/hugo-bare-min-theme/blob/fcb7098652ef386481b5c1f1a390f2d6ad329b6a/layouts/shortcodes/figure.html).


### Custom `figure` shortcode {#custom-figure-shortcode}

Here's a diff of the custom vs inbuilt shortcodes:

```diff
--- Original figure shortcode
+++ Custom figure shortcode
@@ -1,6 +1,6 @@
 <figure {{ with .Get "class" }}class="{{.}}"{{ end }}>
-    {{ with .Get "link"}}<a href="{{.}}">{{ end }}
+    {{ if .Get "link"}}<a href="{{ .Get "link" }}"{{ with .Get "target" }} target="{{ . }}"{{ end }}{{ with .Get "rel" }} rel="{{ . }}"{{ end }}>{{ end }}
         <img src="{{ .Get "src" }}" {{ if or (.Get "alt") (.Get "caption") }}alt="{{ with .Get "alt"}}{{.}}{{else}}{{ .Get "caption" }}{{ end }}" {{ end }}{{ with .Get "width" }}width="{{.}}" {{ end }}{{ with .Get "height" }}height="{{.}}" {{ end }}/>
```

---

Above limitation is not posed if the image is inlined:
<a href="https://orgmode.org/img/org-mode-unicorn-logo.png" target="_self"><img src="https://orgmode.org/img/org-mode-unicorn-logo.png" alt="org-mode-unicorn-logo.png" width="10%" /></a> i.e. the `target`
attribute will be added to the `<a>` tag correctly, as the `figure`
shortcode is not used for inline images.
