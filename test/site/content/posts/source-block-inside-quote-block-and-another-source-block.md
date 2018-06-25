+++
title = "Source block inside quote block, followed by another source block outside"
tags = ["quotes", "backticks", "src-block"]
categories = ["upstream"]
draft = false
+++

_Blackfriday_ Issue #[407](https://github.com/russross/blackfriday/issues/407)

Some text.

> Some quoted text.
>
> ```emacs-lisp
> (message "hello")
> ```

````emacs-lisp
(message "hello again")
````

**1** Some text.

> Some quoted text.
>
> ````emacs-lisp
> (message "hello")
> ````

`````emacs-lisp
(message "hello again")
`````

**2** Some text.

> Some quoted text.
>
> `````emacs-lisp
> (message "hello")
> `````

``````emacs-lisp
(message "hello again")
``````

**3** Some text.

> Some quoted text.
>
> ``````emacs-lisp
> (message "hello")
> ``````

```````emacs-lisp
(message "hello again")
```````

**4** Some text.

> Some quoted text.
>
> ```````emacs-lisp
> (message "hello")
> ```````

````````emacs-lisp
(message "hello again")
````````

**5** Some text.

> Some quoted text.
>
> ````````emacs-lisp
> (message "hello")
> ````````

`````````emacs-lisp
(message "hello again")
`````````

Some other text.
