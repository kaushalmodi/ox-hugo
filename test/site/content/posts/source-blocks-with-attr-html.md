+++
title = "Source blocks with ATTR_HTML"
tags = ["src-block", "attr_html", "attr_css"]
draft = false
+++

Some text.

<style>.indent-block { padding-left: 50px;  }</style>

<div class="indent-block">

```emacs-lisp
(message (mapconcat #'identity
                    '("Hello," "how" "are" "you?")
                    " "))
```
</div>

Some more text.
