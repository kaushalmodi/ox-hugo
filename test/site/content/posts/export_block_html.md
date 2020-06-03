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


## `#+begin_export html` blocks with script tag {#plus-begin-export-html-blocks-with-script-tag}

`ox-hugo` Issue #[369](https://github.com/kaushalmodi/ox-hugo/issues/369)

Check that `src="https://example.com"` does not become
`src="<https://example.com>"`.

Note
: Below iframe/script will **not** work on this site because of
    the strict [CSP](https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP). Anyways. this site is to test just the
    Markdown exported by ox-hugo. So please see "_✱ Markdown
    source of this page_" above.

<!--listend-->

```org
#+begin_export html
<div class="glitch-embed-wrap" style="height: 420px; width: 100%;">
  <iframe
    src="https://glitch.com/embed/#!/embed/silken-football?path=app.py&previewSize=0&sidebarCollapsed=true"
    title="silken-football on Glitch"
    style="height: 100%; width: 100%; border: 0;">
  </iframe>
</div>
#+end_export
```

<div class="glitch-embed-wrap" style="height: 420px; width: 100%;">
  <iframe
    src="https://glitch.com/embed/#!/embed/silken-football?path=app.py&previewSize=0&sidebarCollapsed=true"
    title="silken-football on Glitch"
    style="height: 100%; width: 100%; border: 0;">
  </iframe>
</div>


## To see the exported HTML {#to-see-the-exported-html}

_See the Markdown source of this page to see the verbatim-inserted
HTML._
