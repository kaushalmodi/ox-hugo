+++
title = "Bind footnotes to the preceding word"
aliases = ["/posts/footnotes-at-eol"]
tags = ["footnote"]
draft = false
+++

`ox-hugo` Issue #[96](https://github.com/kaushalmodi/ox-hugo/issues/96)

To test the fix for this, increase/decrease the width of the browser
window showing this page so that the test lines below start wrapping
around, and you will see that the footnote references will **never** be
on their own on a new line.


## Footnote ref at EOL {#footnote-ref-at-eol}


### Last word, followed by FOOTNOTE PERIOD --- _Good Case A_ {#last-word-followed-by-footnote-period-good-case-a}

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


### Last word, followed by FOOTNOTE **space** PERIOD --- _Bad Case A1_ {#last-word-followed-by-footnote-space-period-bad-case-a1}

-   In this case, the **space** before the PERIOD at EOL is removed.

a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a[^fn:1].

ab a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a[^fn:1].

abc a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a[^fn:1].

abcd a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a[^fn:1].


### Last word, followed by PERIOD **space** FOOTNOTE --- _Bad Case A2_ {#last-word-followed-by-period-space-footnote-bad-case-a2}

-   In this case, the **space** before FOOTNOTE is replaced with `&nbsp;`.

a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a.&nbsp;[^fn:1]

ab a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a.&nbsp;[^fn:1]

abc a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a.&nbsp;[^fn:1]

abcd a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a.&nbsp;[^fn:1]


### Last word, followed by **space** FOOTNOTE **space** PERIOD --- _Bad Case A3_ {#last-word-followed-by-space-footnote-space-period-bad-case-a3}

-   This is a blend of _Bad Case A1_ and _Bad Case A2_ above.
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


## Footnote NOT at EOL {#footnote-not-at-eol}


### Word, followed by FOOTNOTE PERIOD --- _Good Case B_ {#word-followed-by-footnote-period-good-case-b}

-   As there is no space in-between "word FOOTNOTE PERIOD", this text
    will stay unmodified.

a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a[^fn:1]. B b b.

ab a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a[^fn:1]. B b b.

abc a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a[^fn:1]. B b b.

abcd a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a[^fn:1].


### Word, followed by FOOTNOTE **space** PERIOD --- _Bad Case B1_ {#word-followed-by-footnote-space-period-bad-case-b1}

-   In this case, the **space** before the PERIOD at EOL is removed.

a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a[^fn:1]. B b b.

ab a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a[^fn:1]. B b b.

abc a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a[^fn:1]. B b b.

abcd a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a[^fn:1]. B b b.


### Word, followed by PERIOD **space** FOOTNOTE --- _Bad Case B2_ {#word-followed-by-period-space-footnote-bad-case-b2}

-   In this case, the **space** before FOOTNOTE is replaced with `&nbsp;`.

a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a.&nbsp;[^fn:1] B b b.

ab a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a.&nbsp;[^fn:1] B b b.

abc a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a.&nbsp;[^fn:1] B b b.

abcd a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a.&nbsp;[^fn:1] B b b.


### Word, followed by **space** FOOTNOTE **space** PERIOD --- _Bad Case B3_ {#word-followed-by-space-footnote-space-period-bad-case-b3}

-   This is a blend of _Bad Case B1_ and _Bad Case B2_ above.
-   In this case, the **space** before FOOTNOTE is replaced with `&nbsp;`,
    **AND** the **space** before the PERIOD at EOL is removed.

a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a&nbsp;[^fn:1]. B b b.

ab a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a&nbsp;[^fn:1]. B b b.

abc a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a&nbsp;[^fn:1]. B b b.

abcd a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a&nbsp;[^fn:1]. B b b.

abcde a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
a a a&nbsp;[^fn:1]. B b b.

[^fn:1]: First footnote
