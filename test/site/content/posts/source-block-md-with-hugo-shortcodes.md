+++
title = "Markdown source block with Hugo shortcodes"
tags = ["src-block", "shortcode"]
draft = false
+++

## Shortcodes escaped {#shortcodes-escaped}

The `figure` shortcodes in the two Markdown source code blocks below
should **not** be expanded.. they should be visible verbatim.

-   {{&lt; .. &gt;}} --- [Shortcodes without Markdown](https://gohugo.io/content-management/shortcodes/#shortcodes-without-markdown)
-   {&lbrace;% .. %&rbrace;} --- [Shortcodes with Markdown](https://gohugo.io/content-management/shortcodes/#shortcodes-with-markdown)


### Code block using code fences {#code-block-using-code-fences}

```md
{{</* figure src="https://ox-hugo.scripter.co/test/images/org-mode-unicorn-logo.png" */>}}
{{%/* figure src="https://ox-hugo.scripter.co/test/images/org-mode-unicorn-logo.png" */%}}
```


### Code block using code fences with line numbering enabled {#code-block-using-code-fences-with-line-numbering-enabled}

Here, the `-n` switch is added to the Org source block to enable line
numbering.

```md { linenos=table, linenostart=1 }
{{</* figure src="https://ox-hugo.scripter.co/test/images/org-mode-unicorn-logo.png" */>}}
{{%/* figure src="https://ox-hugo.scripter.co/test/images/org-mode-unicorn-logo.png" */%}}
```


## Shortcodes **not** escaped {#shortcodes-not-escaped}

The `figure` shortcode in the below example block **should** be
expanded.. you should be seeing a little unicorn below.

```text
{{< figure src="https://ox-hugo.scripter.co/test/images/org-mode-unicorn-logo.png" >}}
```

Above a `#+begin_example` .. `#+end_example` block is chosen
arbitrarily. The Hugo shortcodes will remain unescaped in **any**
source/example block except for <span class="underline">Markdown source blocks</span> (annotated
with `md` language).

Below, the same `figure` shortcode is called with the `%` syntax.

Note
: If you are using Hugo 0.55.0 or newer, you will just see the
    raw HTML from this shortcode (unrendered HTML) because the behavior
    of {&lbrace;% .. %&rbrace;} shortcodes [changed in Hugo v0.55.0](https://gohugo.io/news/0.55.0-relnotes/#shortcodes-revised).

<!--listend-->

```text
{{% figure src="https://ox-hugo.scripter.co/test/images/org-mode-unicorn-logo.png" %}}
```

---

**It is necessary to set the Hugo site config variable
`markup.highlight.codeFences` to `true` (default) for syntax
highlighting to work for fenced code blocks.**
