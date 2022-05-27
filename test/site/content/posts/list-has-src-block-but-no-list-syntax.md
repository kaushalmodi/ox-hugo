+++
title = "Source block without list syntax in a list"
description = "Test source blocks inside list."
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

---

`ox-hugo` Issue #[645](https://github.com/kaushalmodi/ox-hugo/issues/645)

1.  paragraph -&gt; src block with caption -&gt; paragraph -&gt; src block
    <a id="code-snippet--foo"></a>
    ```emacs-lisp
    (message "hey")
    ```
    <div class="src-block-caption">
      <span class="src-block-number"><a href="#code-snippet--foo">Code Snippet 1</a>:</span>
      Foo
    </div>

    sandwiched paragraph
    ```emacs-lisp
    (message "hey")
    ```
    last paragraph
2.  paragraph -&gt; src block **without** caption -&gt; paragraph -&gt; src block
    ```emacs-lisp
    (message "hey")
    ```
    sandwiched paragraph
    ```emacs-lisp
    (message "hey")
    ```
    last paragraph
