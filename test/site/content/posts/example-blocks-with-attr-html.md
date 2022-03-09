+++
title = "Example blocks with ATTR_HTML (Goldmark)"
tags = ["example", "attr_html", "attr_css", "goldmark"]
draft = false
+++

Some text.

<style>.indent-block { padding-left: 50px;  }</style>

```text { class="indent-block" }
This is an example
Line 2
Line 3
```

Some more text.

<style>.heavy { font-weight: bold;  }</style>

```text { class="heavy" title="some code block", linenos=true, linenostart=1 }
This is an example
Line 2
Line 3
```
