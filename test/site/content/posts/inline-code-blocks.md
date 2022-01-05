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

`(message "Hello 2")`


## Both code and results {#both-code-and-results}

```org
src_emacs-lisp[:exports both]{(message "Hello 3")}
```

`(message "Hello 3")` `Hello 3`


## None! {#none}

```org
src_emacs-lisp[:exports none]{(message "Hello 4")}
```
