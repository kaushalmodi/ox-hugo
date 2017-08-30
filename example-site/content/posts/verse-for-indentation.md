+++
title = "Verse for indentation"
tags = ["verse"]
draft = false
+++

Some text before indented text.

&#xa0;&#xa0;&#xa0;&#xa0;Indented text<br />

Org removes indentation from the first line of the text block even in
a Verse block. To get around that, the trick is to use the `>`
character as the first character in the first line containing some
text in a Verse block. That character is removed when translating to
Markdown.

More examples:

-   Even more indentation than int the above example:

    &#xa0;&#xa0;&#xa0;&#xa0;&#xa0;&#xa0;&#xa0;&#xa0;Indented text<br />
-   Verse block containing a leading blank line and then indented text:

    <br />
    &#xa0;&#xa0;&#xa0;&#xa0;Indented text<br />
-   Verse block containing indented text and then a trailing blank line:

    &#xa0;&#xa0;&#xa0;&#xa0;Indented text<br />
    <br />
-   Verse block using tab characters for indentation:

    &#xa0;&#xa0;&#xa0;&#xa0;Indented text<br />
-   `>` characters when used as first character on any Verse block line
    will be removed:

    Line 1<br />
    Line 2<br />
    Line 3<br />
    Line 4<br />
