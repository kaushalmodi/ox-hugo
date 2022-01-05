+++
title = "Example blocks with ATTR_HTML (Blackfriday)"
tags = ["example", "attr_html", "attr_css", "blackfriday"]
draft = false
+++

Some text.

<style>.indent-block { padding-left: 50px;  }</style>

<div class="indent-block">
  <div></div>

```text
This is an example
Line 2
Line 3
```
</div>

Some more text.

<style>.heavy { font-weight: bold;  }</style>

<div class="heavy">
  <div></div>

{{< highlight text "linenos=table, linenostart=1" >}}
This is an example
Line 2
Line 3
{{< /highlight >}}

</div>
