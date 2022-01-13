+++
title = "_build Front-matter"
tags = ["front-matter", "page-bundles", "_build"]
draft = false
+++

`ox-hugo` Issue #[358](https://github.com/kaushalmodi/ox-hugo/issues/358) | [Hugo v0.65.0 release notes](https://gohugo.io/news/0.65.0-relnotes/#new-in-hugo-core)

{{< getpage1.inline >}}
  {{ with site.GetPage "do-not-list" }}
  <h2>"do-not-list"</h2>
  <p>
    Found "do-not-list" page -> <a href="{{ .Permalink }}">{{ .Title }}</a>
  </p>
  <p>
    <strong>Content</strong>:
    <blockquote>{{ .Content }}</blockquote>
  </p>
  {{ end }}
{{< /getpage1.inline >}}

{{< getpage2.inline >}}
  {{ with site.GetPage "do-not-render" }}
  <h2>"do-not-render"</h2>
  <p>
    Found "do-not-render" page -> <a href="{{ .Permalink }}">{{ .Title }}</a> (<mark>As this page is not
    technically rendered, this link doesn't actually link to that page; it just links to its parent page.</mark>)
  </p>
  <p>
    <strong>Content</strong>:
    <blockquote>{{ .Content }}</blockquote>
  {{ end }}
  </p>
{{< /getpage2.inline >}}

---

Note
: An [_inline shortcode_](https://gohugo.io/templates/shortcode-templates/#inline-shortcodes) is used above. `enableInlineShortcodes
           = true` needs to be added to the site config for that to work.
