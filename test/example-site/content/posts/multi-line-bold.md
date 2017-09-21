+++
title = "Multi-line bold"
tags = ["formatting"]
draft = false
+++

**This works fine as the bold sentence does not include a newline.**

**This is a sentence that should render completely in bold. It is
 broken across multiple lines (in Org source) because of
 auto-filling. But that should not break the bold rendering. But it
 does by default.**

If you do not see the above paragraph completely in bold, have below
in your emacs config to fix it:

```emacs-lisp
(with-eval-after-load 'org
  ;; Allow multiple line Org emphasis markup.
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))
```
