+++
title = "Source and example blocks in lists"
description = "Test to verify rendering of source and example blocks in lists."
tags = ["lists", "src-block", "example-block"]
draft = false
+++

[Ref](https://discourse.gohugo.io/t/blackfriday-not-handling-lists-and-code-blocks-the-right-way/19932)

-   list item 1

    ```nim
    echo "hello from list item 1"
    ```

    -   list item 1.1

        ```text
        echo "hello from list item 1.1"
        ```
    -   ```text
        echo "hello from list item 1.2"
        echo "there's not text before this example block in this list item"
        ```

        -   list item 1.2.1

            ```nim
            echo "hello from list item 1.2.1"
            ```
        -   ```nim
            echo "hello from list item 1.2.2"
            echo "there's not text before this src block in this list item"
            ```

            -   list item 1.2.2.1

                ```text
                echo "hello from list item 1.2.2.1"
                ```
-   ```nim
    echo "hello from list item 2"
    echo "there's not text before this src block in this list item"
    ```
-   list item 3

    ```text
    echo "hello from list item 3"
    ```
