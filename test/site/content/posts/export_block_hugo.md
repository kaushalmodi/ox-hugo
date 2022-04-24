+++
title = "Export block Hugo"
description = "Testing `#+begin_export hugo` .. `#+end_export` blocks."
tags = ["export-block", "hugo"]
draft = false
+++

This will get exported **only for** Hugo exports, `verbatim`.


## Using _export hugo_ block for unpaired shortcodes with named arguments {#using-export-hugo-block-for-unpaired-shortcodes-with-named-arguments}

`ox-hugo` Issue #[624](https://github.com/kaushalmodi/ox-hugo/issues/624)
Below:

```org
#+begin_export hugo
{{</* myshortcode-named
      arg1="horizontal"
      arg2="static/img/legacy/works/land"
*/>}}
#+end_export
```

renders as:

{{< myshortcode-named
      arg1="horizontal"
      arg2="static/img/legacy/works/land"
>}}
