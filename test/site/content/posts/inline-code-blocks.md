+++
title = "Inline code blocks"
description = "Test exporting inline code blocks"
tags = ["inline", "code"]
draft = false
+++

[Reference](https://orgmode.org/manual/Structure-of-Code-Blocks.html)


## Only results (default) {#only-results--default}

```org
src_emacs-lisp{(message "Hello 1")}
```

`Hello 1`

Above is the same as:

```org
src_emacs-lisp[:exports results]{(message "Hello 1")}
```

`Hello 1`


## Only code {#only-code}

```org
src_emacs-lisp[:exports code]{(message "Hello 2")}
```

<span class="inline-src language-emacs-lisp" data-lang="emacs-lisp">`(message "Hello 2")`</span>


## Both code and results {#both-code-and-results}

```org
src_emacs-lisp[:exports both]{(message "Hello 3")}
```

<span class="inline-src language-emacs-lisp" data-lang="emacs-lisp">`(message "Hello 3")`</span> `Hello 3`


## None! {#none}

```org
src_emacs-lisp[:exports none]{(message "Hello 4")}
```


## Escape Hugo shortcodes {#escape-hugo-shortcodes}

md
: <span class="inline-src language-md" data-lang="md">`{{</* some_shortcode "foo" */>}}`</span>

org
: <span class="inline-src language-org" data-lang="org">`{{%/* some_shortcode "foo" */%}}`</span>

go-html-template
: <span class="inline-src language-go-html-template" data-lang="go-html-template">`{{</* some_shortcode "foo" */>}}`</span>


## Using custom CSS for inline src blocks {#using-custom-css-for-inline-src-blocks}

`ox-hugo` Issue #[638](https://github.com/kaushalmodi/ox-hugo/issues/638)

CSS used here:

```html
<style>
  .inline-src.language-nim code::before {
        color: initial;
        content: "｢";
    }
  .inline-src.language-nim code::after {
        color: initial;
        content: "｣";
    }
</style>
```

<style>
    .inline-src.language-nim code::before {
        color: initial;
        content: "｢";
    }
    .inline-src.language-nim code::after {
        color: initial;
        content: "｣";
    }
</style>

`ox-hugo` Issue #[640](https://github.com/kaushalmodi/ox-hugo/issues/640) -- Test that straight quotes in inline src
blocks don't get rendered as curved quotes by Hugo/Goldmark's
Typographer.

In Nim, <span class="inline-src language-nim" data-lang="nim">`echo "hello"`</span> will print
_hello_.
