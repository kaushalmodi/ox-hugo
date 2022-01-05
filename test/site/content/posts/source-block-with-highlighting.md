+++
title = "Source blocks with highlighting (Goldmark)"
aliases = ["/posts/source-block-with-highlighting-goldmark"]
tags = ["src-block", "annotations", "goldmark", "highlight"]
draft = false
+++

## Without line numbers {#source-blocks-with-highlighting-no-linenums}


#### Org source {#org-source}

```org
#+begin_src emacs-lisp :hl_lines 1,3-5
(message "This is line 1")
(message "This is line 2")
(message "This is line 3")
(message "This is line 4")
(message "This is line 5")
(message "This is line 6")
#+end_src
```


#### Output {#output}

```emacs-lisp { hl_lines=["1","3-5"] }
(message "This is line 1")
(message "This is line 2")
(message "This is line 3")
(message "This is line 4")
(message "This is line 5")
(message "This is line 6")
```

Above highlighting might look weird as the highlighting spans the full
page/container width. This could be either called a bug in Hugo, or
the HTML limitation.

A workaround is below.. **use line numbers too!**.


#### Highlighting only 1 line {#highlighting-only-1-line}


##### Org source {#org-source}

```org
#+begin_src emacs-lisp :hl_lines 2
(message "This is line 1")
(message "This is line 2")
(message "This is line 3")
#+end_src
```


##### Output {#output}

```emacs-lisp { hl_lines=["2"] }
(message "This is line 1")
(message "This is line 2")
(message "This is line 3")
```


## With line numbers **not** starting from 1 {#source-blocks-with-highlighting-with-linenums-not-starting-from-1}

With line numbers enabled, the highlighting is limited to the width of
the HTML table rows if the `linenos` option is set to `table`
(default).

Note 1
: When using both, switches (like `-n`), and header args
    (like `:hl_lines`), the <span class="underline">switches have to come first</span>.

Note 2
: The line numbers in the value for `:hl_lines` parameter is
    always with the starting line number reference of 1. That
    has no relation with the value of the line numbers
    displayed using the `-n` or `+n` switches!


#### Org source {#org-source}

```org
#+begin_src emacs-lisp -n 7 :hl_lines 1,3-5
(message "This is line 7 in code, but line 1 for highlighting reference")
(message "This is line 8 in code, but line 2 for highlighting reference")
(message "This is line 9 in code, but line 3 for highlighting reference")
(message "This is line 10 in code, but line 4 for highlighting reference")
(message "This is line 11 in code, but line 5 for highlighting reference")
(message "This is line 12 in code, but line 6 for highlighting reference")
#+end_src
```


#### Output {#output}

```emacs-lisp { linenos=table, linenostart=7, hl_lines=["1","3-5"] }
(message "This is line 7 in code, but line 1 for highlighting reference")
(message "This is line 8 in code, but line 2 for highlighting reference")
(message "This is line 9 in code, but line 3 for highlighting reference")
(message "This is line 10 in code, but line 4 for highlighting reference")
(message "This is line 11 in code, but line 5 for highlighting reference")
(message "This is line 12 in code, but line 6 for highlighting reference")
```


## With line numbers {#source-blocks-with-highlighting-with-linenums}


#### Org source {#org-source}

```org
#+begin_src emacs-lisp -n :hl_lines 1,3-5
(message "This is line 1")
(message "This is line 2")
(message "This is line 3")
(message "This is line 4")
(message "This is line 5")
(message "This is line 6")
#+end_src
```


#### Output {#output}

```emacs-lisp { linenos=table, linenostart=1, hl_lines=["1","3-5"] }
(message "This is line 1")
(message "This is line 2")
(message "This is line 3")
(message "This is line 4")
(message "This is line 5")
(message "This is line 6")
```
