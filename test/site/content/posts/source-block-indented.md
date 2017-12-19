+++
title = "Indented source block"
tags = ["src-block"]
draft = false
+++

Test that indented source blocks export fine.

```emacs-lisp
(defun small-shell ()
    (interactive)
    (split-window-vertically)
    (other-window 1)
    (shrink-window (- (window-height) 12))
(ansi-term))
```


## More tests! {#more-tests}

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

Reference: `hugo` Issue #[4006](https://github.com/gohugoio/hugo/issues/4006)
