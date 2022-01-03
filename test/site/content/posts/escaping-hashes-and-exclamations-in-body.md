+++
title = "Escaping hashes and exclamations correctly in body"
tags = ["body", "escaping"]
draft = false
+++

I intend to show these # characters verbatim; they should not render
as Markdown headings. They also shouldn't show up with a leading `\`
in the final rendered output.

\#This is not an Org comment. It has a hash char at beginning of a
paragraph which must be escaped just once i.e. show up as `\#` in
Markdown.

blah # This isn't an Org comment either

This \* will be escaped just once i.e. show up as `\*` in Markdown.

This _ will not be escaped as it's a solo underscore character. But
the following underscore will be escaped: something\_.

This \\ will be escaped just once i.e. show up as `\\` in Markdown.

Hash char at beginning of a continued line
\#like this must be escaped just once i.e. show up as `\#` in Markdown.

\![this exclamation must be escaped just once i.e. show up as `\!` in
Markdown]

This ! does not need to be escaped as there is no ambiguity.
