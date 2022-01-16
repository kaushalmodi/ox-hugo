+++
title = "No TOC in Summary (with more comment marker)"
tags = ["export-option", "toc", "summary", "endtoc", "more"]
draft = false
+++

<div class="ox-hugo-toc toc">

<div class="heading">Table of Contents</div>

- [`ox-hugo`'s Solution](#ox-hugo-s-solution)
- [Snippet](#snippet)

</div>
<!--endtoc-->

By default, Hugo will dump everything at the beginning of a post into
its `.Summary` (See [Hugo content summaries](https://gohugo.io/content-management/summaries/)). As TOC enabled using the
export option like `toc:t` is inserted at the beginning of a post, TOC
will be shown in that summary too!

In this example, the special comment `more` **is used**.

<!--more-->


## `ox-hugo`'s Solution {#ox-hugo-s-solution}

`ox-hugo` helps prevent that with a workaround.. it inserts a special
HTML comment **`<!--endtoc-->`** after the TOC.


## Snippet {#snippet}

As the `more` comment is present, the [`summary_minus_toc.html`](https://github.com/kaushalmodi/hugo-bare-min-theme/blob/master/layouts/partials/summary_minus_toc.html) partial
used for this test site ensures that only the content between
`<!--endtoc-->` and `<!--more-->` is considered as summary.

This partial needs to be used wherever the summary text is needed
(example: the Opengraph `og:description` meta tag).
