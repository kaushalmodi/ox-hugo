+++
title = "Export block HTML"
tags = ["export-block", "html"]
draft = false
+++

This HTML <b>Export Block</b>  will also get exported for Hugo exports,
<code>verbatim</code>.

And Markdown emphasis characters like *, `  and _ will not escaped in here.

From `ox-hugo` Issue #[154](https://github.com/kaushalmodi/ox-hugo/issues/154), we see an actual case of where that `*`
will need to remain unescaped:

```org
# Org source
#+begin_export html
<div>
  <script type="text/javascript">
    var a = 3;
    var b = 2*a;
  </script>
</div>
#+end_export
```

<div>
  <script type="text/javascript">
    var a = 3;
    var b = 2*a;
  </script>
</div>

_See the Markdown source of this page to see the verbatim-inserted
HTML._
