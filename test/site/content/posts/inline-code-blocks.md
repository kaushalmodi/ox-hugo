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

<code class="inline-src language-emacs-lisp" data-lang="emacs-lisp">(message "Hello 2")</code>


## Both code and results {#both-code-and-results}

```org
src_emacs-lisp[:exports both]{(message "Hello 3")}
```

<code class="inline-src language-emacs-lisp" data-lang="emacs-lisp">(message "Hello 3")</code> `Hello 3`


## None! {#none}

```org
src_emacs-lisp[:exports none]{(message "Hello 4")}
```


## Escape Hugo shortcodes {#escape-hugo-shortcodes}

md
: <code class="inline-src language-md" data-lang="md">{{</* some_shortcode "foo" */>}}</code>

org
: <code class="inline-src language-org" data-lang="org">{{%/* some_shortcode "foo" */%}}</code>

go-html-template
: <code class="inline-src language-go-html-template" data-lang="go-html-template">{{</* some_shortcode "foo" */>}}</code>


## Using custom CSS for inline src blocks {#using-custom-css-for-inline-src-blocks}

`ox-hugo` Issue #[638](https://github.com/kaushalmodi/ox-hugo/issues/638)

CSS used here:

```html
<style>
    code.inline-src.language-nim::before {
        color: initial;
        content: "｢";
    }
    code.inline-src.language-nim::after {
        color: initial;
        content: "｣";
    }
</style>
```

<style>
    code.inline-src.language-nim::before {
        color: initial;
        content: "｢";
    }
    code.inline-src.language-nim::after {
        color: initial;
        content: "｣";
    }
</style>

In Nim, <code class="inline-src language-nim" data-lang="nim">echo "hello"</code> will print
_hello_.
