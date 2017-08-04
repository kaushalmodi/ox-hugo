+++
title = "Source block with list syntax in a list"
date = 2017-08-01
tags = ["src_block", "upstream"]
categories = ["fixme"]
draft = false
+++

As of today (<span class="timestamp-wrapper"><span class="timestamp">&lt;2017-08-02 Wed&gt;</span></span>), an upstream bug in _Blackfriday_
([Issue #239](https://github.com/russross/blackfriday/issues/239)) causes fenced code blocks in lists to not render
correctly if they contain Markdown syntax lists.

Below is an example of such a case:

-   List item 1

    ```md
    - List item 1 in code block
    - List item 2 in code block
    ```
-   List item 2
-   List item 3
