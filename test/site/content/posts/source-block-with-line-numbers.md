+++
title = "Source blocks with line number annotation (Goldmark)"
aliases = ["/posts/source-block-with-line-numbers-goldmark"]
tags = ["src-block", "annotations", "goldmark", "linenum"]
draft = false
+++

-   [Org reference](https://orgmode.org/manual/Literal-examples.html)


## Cases {#source-block-line-number-cases}


### Default new line number start {#default-new-line-number-start}


#### Org source {#org-source}

```org
#+begin_src emacs-lisp -n
;; this will export with line number 1 (default)
(message "This is line 2")
#+end_src
```


#### Output {#output}

```emacs-lisp { linenos=table, linenostart=1 }
;; this will export with line number 1 (default)
(message "This is line 2")
```


### Specify new line number start {#specify-new-line-number-start}


#### Org source {#org-source}

```org
#+begin_src emacs-lisp -n 20
;; this will export with line number 20
(message "This is line 21")
#+end_src
```


#### Output {#output}

```emacs-lisp { linenos=table, linenostart=20 }
;; this will export with line number 20
(message "This is line 21")
```


### Default continued line numbers {#default-continued-line-numbers}


#### Org source {#org-source}

```org
#+begin_src emacs-lisp +n
;; This will be listed as line 22
(message "This is line 23")
#+end_src
```


#### Output {#output}

```emacs-lisp { linenos=table, linenostart=22 }
;; This will be listed as line 22
(message "This is line 23")
```


### Specify continued line numbers jump {#specify-continued-line-numbers-jump}


#### Org source {#org-source}

```org
#+begin_src emacs-lisp +n 10
;; This will be listed as line 33
(message "This is line 34")
#+end_src
```


#### Output {#output}

```emacs-lisp { linenos=table, linenostart=33 }
;; This will be listed as line 33
(message "This is line 34")
```


## Specifying `linenos` parameter {#specifying-linenos-parameter}

```emacs-lisp { linenos=false }
(message "This is line 1")
(message "This is line 2")
(message "This is line 3")
```

```emacs-lisp { linenos=inline, linenostart=30 }
(message "This is line 30")
(message "This is line 31")
(message "This is line 32")
```
