+++
title = "Issue 433 – Source block in a footnote"
description = "Source block in an Org footnote"
tags = ["issues", "footnote", "src-block"]
draft = false
+++

`ox-hugo` Issue #[433](https://github.com/kaushalmodi/ox-hugo/issues/433)

Testing code in a footnote with a `begin_src` directive.[^fn:1].

**This doesn't work because Hugo does not support having multi-line
content in footnotes.**

Due to that limitation, `ox-hugo` folds all footnote content onto a
single line.. and so the below Org footnote:

```org
[fn:1]
#+begin_src elisp
(emacs-version)
#+end_src
```

gets exported as below in the Markdown footnote:

```md
[^fn:1]: ```elisp (emacs-version) ```
```

[^fn:1]: ```elisp (emacs-version) ```
