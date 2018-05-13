+++
title = "Indented source block"
description = """
  Test that indented source blocks, and also the ones in lists export
  fine.
  """
tags = ["src-block", "indented", "lists", "code-fence", "highlight", "code-block"]
categories = ["upstream"]
draft = false
+++

Some content.

```emacs-lisp
(defun small-shell ()
    (interactive)
    (split-window-vertically)
    (other-window 1)
    (shrink-window (- (window-height) 12))
(ansi-term))
```


## Code blocks in list using code fences {#code-blocks-in-list-using-code-fences}

Reference: `hugo` Issue #[4006](https://github.com/gohugoio/hugo/issues/4006)

-   List item 1

    ```emacs-lisp
    (message "I am in list at level-1 indentation")
    ```

    -   List item 1.1

        ```emacs-lisp
        (message "I am in list at level-2 indentation")
        ```

        -   List item 1.1.1

            ```emacs-lisp
            (message "I am in list at level-3 indentation")
            ```
    -   List item 2.1

        ```emacs-lisp
        (message "I am in list back at level-2 indentation")
        ```
-   List item 2

    ```emacs-lisp
    (message "I am in list back at level-1 indentation")
    ```

```emacs-lisp
(message "And now I am at level-0 indentation")
```


## Code blocks in list using `highlight` shortcode {#code-blocks-in-list-using-highlight-shortcode}

Reference: `hugo` Issue #[4717](https://github.com/gohugoio/hugo/issues/4717), `ox-hugo` Issue #[161](https://github.com/kaushalmodi/ox-hugo/issues/161)

This is an **upstream** bug in `hugo` as of 2018-05-12. The issues is
that when the code blocks in `highlight` shortcodes are inserted at
the required indentation levels in lists.. so that they get rendered
**in** the list at **that** indentation level, those indentations are not
removed by `hugo`, and thus become part of those code blocks.

Also, related to this issue, it can be seen that all such indented
code blocks have an empty second line too, probably just due to the
unremoved indentation on the last line of those code blocks.

In the above section, the same code blocks are code-fenced instead of
using `highlight` shortcode, and the extra indentation is not seen
there.

-   List item 1

    {{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
    (message "I am in list at level-1 indentation")
    {{< /highlight >}}

    -   List item 1.1

        {{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
        (message "I am in list at level-2 indentation")
        {{< /highlight >}}

        -   List item 1.1.1

            {{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
            (message "I am in list at level-3 indentation")
            {{< /highlight >}}
    -   List item 2.1

        {{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
        (message "I am in list back at level-2 indentation")
        {{< /highlight >}}
-   List item 2

    {{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
    (message "I am in list back at level-1 indentation")
    {{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(message "And now I am at level-0 indentation")
{{< /highlight >}}
