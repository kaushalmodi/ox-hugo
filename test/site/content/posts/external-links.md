+++
title = "External Links"
tags = ["links", "external-links"]
draft = false
+++

-   [Emacs download (FTP)](ftp://ftp.gnu.org/gnu/emacs/)
-   [Org mode (HTTPS)](https://orgmode.org)
-   [Dummy http link](http://some.insecure.site)


## Gopher link support {#gopher-link-support}

`ox-hugo` Issue #[132](https://github.com/kaushalmodi/ox-hugo/issues/132)

The _gopher_ link support requires user to add something like this to
their user config:

```emacs-lisp
(with-eval-after-load 'org
  (defun org-link-gopher-export-link (link desc format)
    "Create export version of LINK and DESC to FORMAT."
    (let ((link (concat "gopher:" link)))
      (cond
       ((eq format 'html)
        (format "<a href=\"%s\">%s</a>" link desc))
       ((eq format 'latex)
        (format "\\href{%s}{%s}" link desc))
       (t                               ;`ascii', `md', `hugo', etc.
        (format "[%s](%s)" desc link)))))
  (org-link-set-parameters "gopher" :export #'org-link-gopher-export-link))
```

Once that is evaluated, links like these will export fine i.e. no
"Unable to resolve link" errors:

```org
[[gopher://some.gopher.site][Dummy gopher link]]
```


## Mailto link support {#mailto-link-support}

`ox-hugo` Issue #[149](https://github.com/kaushalmodi/ox-hugo/issues/149)

[My email](mailto:abc@xyz.com)
