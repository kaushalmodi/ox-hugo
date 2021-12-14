+++
title = "Escaping angle brackets"
description = "Test the escaping of angle brackets because they are used in HTML tags."
tags = ["body", "escaping", "angle-brackets"]
draft = false
+++

-   some &lt;text&gt;
-   some &lt; _italic text_ &gt; (spaces are required between the italics
    formatting chars and `<` / `>`)

If you really need to put verbatim HTML in Org source, use
`#+begin_export html` block:

<ul>
  <li><b>This is in bold</b></li>
  <li><i>This is in italics</i></li>
</ul>

This also works if you _desperately need_ to inline HTML within your
Org source: <b>This is bold</b> .. But why<span
style="color:red;">‽</span>.
