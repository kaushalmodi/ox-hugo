+++
title = "Headless Page Bundle Index Page"
tags = ["page-bundles", "headless"]
draft = false
headless = true
+++

This is a **headless** page bundle. This feature was introduced in [this
commit](https://github.com/gohugoio/hugo/commit/0432c64dd22e4610302162678bb93661ba68d758), and available in Hugo v0.35+.

As this bundle is headless, the **index** page of this bundle (this
page!) will not be published anywhere:

-   It will have no `Permalink` and no rendered HTML in `public/`.
-   It will not be part of `.Site.RegularPages`, etc.

But you can get it by `.Site.GetPage ...`. Here is an example ([ref](https://github.com/gohugoio/hugo/issues/4311#issuecomment-359461045)):

```text
{{ $headless := .Site.GetPage "page" "some-headless-page" }}
{{ $reusablePages := $headless.Resources.Match "sidebar-content*" }}
{{ range $reusablePages }}
    {{ .Title }}
{{ end }}
```

There are many use cases of such headless page bundles:

-   Shared media galleries
-   Reusable page content "snippets"
