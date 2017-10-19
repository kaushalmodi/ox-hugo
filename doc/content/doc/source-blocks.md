+++
title = "Source blocks"
draft = false
[menu."org_to_hugo"]
  weight = 3004
  identifier = "source-blocks"
+++

`ox-hugo` supports exporting source blocks with line numbers and/or
highlighting enabled for specific lines.


## Line numbers {#line-numbers}

Line numbers can be enabled/configured using the Org `-n` / `+n`
syntax. See the Info node [`(org) Literal examples`](http://orgmode.org/manual/Literal-examples.html) for more
information.

Here are some examples fetched from the "Source blocks with line
number annotation" test case in the [`all-posts.org`](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content-org/all-posts.org).


### Default new line number start {#default-new-line-number-start}


#### Org source {#org-source}

```org
#+BEGIN_SRC emacs-lisp -n
;; this will export with line number 1 (default)
(message "This is line 2")
#+END_SRC
```


#### Output {#output}

{{< highlight emacs-lisp "linenos=table, linenostart=1">}}
;; this will export with line number 1 (default)
(message "This is line 2")
{{< /highlight >}}


### Specify new line number start {#specify-new-line-number-start}


#### Org source {#org-source}

```org
#+BEGIN_SRC emacs-lisp -n 20
;; this will export with line number 20
(message "This is line 21")
#+END_SRC
```


#### Output {#output}

{{< highlight emacs-lisp "linenos=table, linenostart=20">}}
;; this will export with line number 20
(message "This is line 21")
{{< /highlight >}}


### Default continued line numbers {#default-continued-line-numbers}


#### Org source {#org-source}

```org
#+BEGIN_SRC emacs-lisp +n
;; This will be listed as line 22
(message "This is line 23")
#+END_SRC
```


#### Output {#output}

{{< highlight emacs-lisp "linenos=table, linenostart=22">}}
;; This will be listed as line 22
(message "This is line 23")
{{< /highlight >}}


### Specify continued line numbers jump {#specify-continued-line-numbers-jump}


#### Org source {#org-source}

```org
#+BEGIN_SRC emacs-lisp +n 10
;; This will be listed as line 33
(message "This is line 34")
#+END_SRC
```


#### Output {#output}

{{< highlight emacs-lisp "linenos=table, linenostart=33">}}
;; This will be listed as line 33
(message "This is line 34")
{{< /highlight >}}


## Highlighting {#highlighting}

Implementing this feature was interesting, because while Org doesn't
have a syntax to enable highlighting only specific lines, the Hugo
`highlight` shortcode does allow that via `hl_lines` argument.

So the challenge was to present that "lines to be highlighted"
information in the Org source in a nice format and then translate that
to the `hl_lines` `highlight` shortcode argument at the time of
exporting.

It involved _hacking_ the `org-babel-exp-code`. See [this discussion on
the `emacs-orgmode` thread](http://lists.gnu.org/archive/html/emacs-orgmode/2017-10/msg00300.html) if interested.

This feature is implemented by using a parameter called `:hl_lines` in
the header of source blocks. This parameter is specific to `ox-hugo`,
and that's why implementing this needed that hack.

If a user wants to highlight lines 1, and then 3 to 5, they would add
`:hl_lines 1,3-5` to the source block header.


### Without line numbers {#without-line-numbers}


#### Org source {#org-source}

```org
#+BEGIN_SRC emacs-lisp :hl_lines 1,3-5
(message "This is line 1")
(message "This is line 2")
(message "This is line 3")
(message "This is line 4")
(message "This is line 5")
(message "This is line 6")
#+END_SRC
```


#### Output {#output}

{{< highlight emacs-lisp "hl_lines=1 3-5">}}
(message "This is line 1")
(message "This is line 2")
(message "This is line 3")
(message "This is line 4")
(message "This is line 5")
(message "This is line 6")
{{< /highlight >}}

Above highlighting might look weird as the highlighting spans the full
page/container width. This could be either called a bug in Hugo, or
the HTML limitation.

A workaround is below.. **use line numbers too!**.


### With line numbers {#with-line-numbers}

The Org source for the below is similar to the above, except that the
`-n` switch is also added to enable the line numbers.

With line numbers enabled, the highlighting is limited to the width of
the HTML table rows (because `ox-hugo` sets the `linenos=table` option
in the `highlight` shortcode when line numbers are enabled).

-   **Note 1:** When using both, switches (like `-n`), and header args
    (like `:hl_lines`), the <span class="underline">switches have to come first</span>.
-   **Note 2:** The line numbers in the value for `:hl_lines` parameter is
    always with the starting line number reference of 1. That
    has no relation with the value of the line numbers
    displayed using the `-n` or `+n` switches!


#### Org source {#org-source}

```org
#+BEGIN_SRC emacs-lisp -n 7 :hl_lines 1,3-5
(message "This is line 7 in code, but line 1 for highlighting reference")
(message "This is line 8 in code, but line 2 for highlighting reference")
(message "This is line 9 in code, but line 3 for highlighting reference")
(message "This is line 10 in code, but line 4 for highlighting reference")
(message "This is line 11 in code, but line 5 for highlighting reference")
(message "This is line 12 in code, but line 6 for highlighting reference")
#+END_SRC
```


#### Output {#output}

{{< highlight emacs-lisp "linenos=table, linenostart=7, hl_lines=1 3-5">}}
(message "This is line 7 in code, but line 1 for highlighting reference")
(message "This is line 8 in code, but line 2 for highlighting reference")
(message "This is line 9 in code, but line 3 for highlighting reference")
(message "This is line 10 in code, but line 4 for highlighting reference")
(message "This is line 11 in code, but line 5 for highlighting reference")
(message "This is line 12 in code, but line 6 for highlighting reference")
{{< /highlight >}}
