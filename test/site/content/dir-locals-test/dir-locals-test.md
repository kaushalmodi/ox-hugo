---
title: ".dir-locals.el test"
description: "Test setting of few Org Hugo variables via `.dir-locals.el`."
date: 2018-10-31
tags: ["dir-locals"]
draft: false
creator: "Dummy creator string"
---

<style>
  .ox-hugo-toc ul {
    list-style: none;
  }
</style>
<div class="ox-hugo-toc toc">

<div class="heading">Table of Contents</div>

- <span class="section-num">1</span> [Variables set in <kbd>.dir-locals.el</kbd>](#variables-set-in-dot-dir-locals-dot-el)
- <span class="section-num">2</span> [Test text](#test-text)

</div>
<!--endtoc-->



## <span class="section-num">1</span> Variables set in <kbd>.dir-locals.el</kbd> {#variables-set-in-dot-dir-locals-dot-el}

<a id="table--vars-dir-locals"></a>
<div class="table-caption">
  <span class="table-number"><a href="#table--vars-dir-locals">Table 1</a></span>:
  Variables set using <code>.dir-locals.el</code>
</div>

| Variable                               | Value                                                                   |
|----------------------------------------|-------------------------------------------------------------------------|
| `org-hugo-base-dir`                    | `"../../."`                                                             |
| `org-hugo-section`                     | `"dir-locals-test"`                                                     |
| `org-hugo-front-matter-format`         | `"yaml"`                                                                |
| `org-hugo-footer`                      | `"\n\nThis text is auto inserted at the end of the exported Markdown."` |
| `org-hugo-preserve-filling`            | `nil`                                                                   |
| `org-hugo-use-code-for-kbd`            | `t`                                                                     |
| `org-hugo-export-with-toc`             | `t`                                                                     |
| `org-hugo-export-with-section-numbers` | `t`                                                                     |
| `org-hugo-export-creator-string`       | `"Dummy creator string"`                                                |
| `org-hugo-date-format`                 | `"%Y-%m-%d"`                                                            |


## <span class="section-num">2</span> Test text {#test-text}

`This is verbatim` but <kbd>this</kbd> is wrapped in the `kbd` tag. As `org-hugo-preserve-filling` is set to `nil`, the column filling in the Org source is not preserved in the exported Markdown.

This text is auto inserted at the end of the exported Markdown.
