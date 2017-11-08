+++
title = "Source block inside quote block, followed by another source block outside"
tags = ["quotes"]
draft = false
+++

[Blackfriday Issue # 407](https://github.com/russross/blackfriday/issues/407)

Some text.

> Some quoted text.
>
> ```emacs-lisp
> (message "hello")
> ```

````emacs-lisp
(message "hello again")
````

Some other text.
