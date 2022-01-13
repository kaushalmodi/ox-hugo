+++
title = "Verse for indentation"
tags = ["verse"]
draft = false
+++

Some text before indented text.

<p class="verse">

&nbsp;&nbsp;&nbsp;&nbsp;Text indented by 4 spaces<br />

</p>

Org removes indentation from the first line of the text block even in
a Verse block. To get around that, the trick is to use the `>`
character before the required indentation spaces **only** on the first
non-blank line in a Verse block. Only that first `>` character is
removed when translating to Markdown.


## More examples {#more-examples}

-   More indentation than in the above example:

    <p class="verse">

    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Text indented by 8 spaces<br />

    </p>
-   Leading blank line followed by indented text:

    <p class="verse">

    <br />
    &nbsp;&nbsp;&nbsp;&nbsp;Text indented by 4 spaces<br />

    </p>
-   Indented text followed by a trailing blank line:

    <p class="verse">

    &nbsp;&nbsp;&nbsp;&nbsp;Text indented by 4 spaces<br />
    <br />

    </p>
-   Using tab characters for indentation; each tab character still
    constitutes for one `&nbsp;` in HTML.

    <p class="verse">

    &nbsp;&nbsp;&nbsp;&nbsp;Text indented by 4 tab characters<br />

    </p>


## Corner cases {#corner-cases}

```org { linenos=table, linenostart=0 }
#+begin_verse

>Line 1 above was empty. So the first =>= seen on this line is removed.
Line 3 had no =>= char.
> ← See that this =>= on line 4 is retained even at the beginning of the line.
Line 5 has this > charcter in-between and is retained.
#+end_verse
```

Only the **first** `>` character immediately following spaces and empty
lines will be removed:

<p class="verse">

<br />
Line 1 above was empty. So the first `>` seen on this line is removed.<br />
Line 3 had no `>` char.<br />
&gt; ← See that this `>` on line 4 is retained even at the beginning of the line.<br />
Line 5 has this &gt; charcter in-between and is retained.<br />

</p>

If someone really wants to have `>` as the first non-blank character
in the final output, they can use `>>` instead.. **only for that first
instance**. The below Verse block is same as above except that the
first `>` is retained in the final output.

```org { linenos=table, linenostart=0 }
#+begin_verse

>>Line 1 above was empty. So *only* the first =>= seen on this line is removed.
Line 3 had no =>= char.
> ← See that this =>= on line 4 is retained even at the beginning of the line.
Line 5 has this > charcter in-between and is retained.
#+end_verse
```

<p class="verse">

<br />
&gt;Line 1 above was empty. So **only** the first `>` seen on this line is removed.<br />
Line 3 had no `>` char.<br />
&gt; ← See that this `>` on line 4 is retained even at the beginning of the line.<br />
Line 5 has this &gt; charcter in-between and is retained.<br />

</p>
