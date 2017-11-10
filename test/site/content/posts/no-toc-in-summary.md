+++
title = "No TOC in Summary"
tags = ["export-option", "toc"]
draft = false
+++

<div class="ox-hugo-toc toc">
<div></div>

## Table of Contents

- [`ox-hugo`'s Solution](#ox-hugo-s-solution)
- [Snippet](#snippet)
- [Example](#example)
</div>
<!--endtoc-->

By default, Hugo will dump everything at the beginning of a post into
its `.Summary` (See [Hugo content summaries](https://gohugo.io/content-management/summaries/)). As TOC enabled using the
export option like `toc:t` is inserted at the beginning of a post, TOC
will be shown in that summary too!

<!--more-->


## `ox-hugo`'s Solution {#ox-hugo-s-solution}

`ox-hugo` helps prevent that with a workaround.. it inserts a special
HTML comment **`<!--endtoc-->`** after the TOC.

It is important to insert a user-defined summary split by using
`#+HUGO: more`. Otherwise it is very likely that the TOC is big enough
to exceed the Hugo-defined max-summary length and so the
`<!--endtoc-->` that appears **after** the TOC never gets parsed.

> **Always use `#+HUGO: more` when you enable Org generated TOC's.**

In your site's Hugo template, you can then filter that out with
something like:


## Snippet {#snippet}

```html
{{ $summary_splits := split .Summary "<!--endtoc-->" }}
{{ if eq (len $summary_splits) 2 }}
    <!-- If that endtoc special comment is present, output only the part after that comment as Summary. -->
    {{ index $summary_splits 1 | safeHTML }}
{{ else }}
    <!-- Print the whole Summary if endtoc special comment is not found. -->
    {{ .Summary }}
{{ end }}
```


## Example {#example}

See this test site's [`summary.html`](https://github.com/kaushalmodi/ox-hugo/blob/master/test/site/themes/bare_min/layouts/_default/summary.html) as an example.
