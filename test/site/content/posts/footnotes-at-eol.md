+++
title = "Footnotes at End of line"
tags = ["footnote"]
draft = false
+++

See [Issue#96](https://github.com/kaushalmodi/ox-hugo/issues/96).

To test the fix for this, increase/decrease the width of the browser
window showing this page so that the test lines below start wrapping
around, and you will see that the footnote references will **never** be
on their own on a new line.


## Last word, followed by FOOTNOTE PERIOD --- _Good Case_ {#last-word-followed-by-footnote-period-good-case}

-   As there is no space in-between "word FOOTNOTE PERIOD", this text
    will stay unmodified.

a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a[^fn:1].

ab a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a[^fn:1].

abc a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a[^fn:1].

abcd a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a[^fn:1].


## Last word, followed by FOOTNOTE **space** PERIOD --- _Bad Case 1_ {#last-word-followed-by-footnote-space-period-bad-case-1}

-   In this case, the **space** before the PERIOD at EOL is removed.

a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a[^fn:1].

ab a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a[^fn:1].

abc a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a[^fn:1].

abcd a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a[^fn:1].


## Last word, followed by PERIOD **space** FOOTNOTE --- _Bad Case 2_ {#last-word-followed-by-period-space-footnote-bad-case-2}

-   In this case, the **space** before FOOTNOTE is replaced with `&nbsp;`.

a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a.&nbsp;[^fn:1]

ab a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a.&nbsp;[^fn:1]

abc a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a.&nbsp;[^fn:1]

abcd a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a.&nbsp;[^fn:1]


## Last word, followed by **space** FOOTNOTE **space** PERIOD --- _Bad Case 3_ {#last-word-followed-by-space-footnote-space-period-bad-case-3}

-   This is a blend of _Bad Case 1_ and _Bad Case 2_ above.
-   In this case, the **space** before FOOTNOTE is replaced with `&nbsp;`,
    **AND** the **space** before the PERIOD at EOL is removed.

a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a&nbsp;[^fn:1].

ab a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a&nbsp;[^fn:1].

abc a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a&nbsp;[^fn:1].

abcd a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a&nbsp;[^fn:1].

abcde a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a&nbsp;[^fn:1].

[^fn:1]: First footnote
