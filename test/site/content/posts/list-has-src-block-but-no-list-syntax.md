+++
title = "Source block without list syntax in a list"
date = 2017-08-01
tags = ["src-block", "lists", "hyphen"]
draft = false
+++

This case is not affected by _Blackfriday_ [Issue #239](https://github.com/russross/blackfriday/issues/239) as the fenced
code block does not have Markdown syntax lists.

-   List item 1

    ```md
    *abc*
    /def/
    =def=
    ```
-   List item 2
