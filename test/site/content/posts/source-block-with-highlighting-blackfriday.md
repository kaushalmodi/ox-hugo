+++
title = "Source blocks with highlighting (Blackfriday)"
tags = ["src-block", "shortcode", "blackfriday", "highlight"]
draft = false
+++

## Without line numbers {#without-line-numbers}


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

{{< highlight emacs-lisp "hl_lines=1 3-5" >}}
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

{{< highlight emacs-lisp "hl_lines=2" >}}
(message "This is line 1")
(message "This is line 2")
(message "This is line 3")
{{< /highlight >}}


## With line numbers **not** starting from 1 {#with-line-numbers-not-starting-from-1}


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

{{< highlight emacs-lisp "linenos=table, linenostart=7, hl_lines=1 3-5" >}}
(message "This is line 7 in code, but line 1 for highlighting reference")
(message "This is line 8 in code, but line 2 for highlighting reference")
(message "This is line 9 in code, but line 3 for highlighting reference")
(message "This is line 10 in code, but line 4 for highlighting reference")
(message "This is line 11 in code, but line 5 for highlighting reference")
(message "This is line 12 in code, but line 6 for highlighting reference")
{{< /highlight >}}


## With line numbers {#with-line-numbers}


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

{{< highlight emacs-lisp "linenos=table, linenostart=1, hl_lines=1 3-5" >}}
(message "This is line 1")
(message "This is line 2")
(message "This is line 3")
(message "This is line 4")
(message "This is line 5")
(message "This is line 6")
{{< /highlight >}}
