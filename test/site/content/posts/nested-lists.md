+++
title = "Nested lists"
date = 2017-07-31
tags = ["lists"]
categories = ["upstream"]
draft = false
+++

-   L1 -- foo1
-   L1 -- foo2
    -   L2 -- bar1
    -   L2 -- bar2
        -   L3 -- baz1
        -   L3 -- baz2
            -   L4 -- zoo1
            -   L4 -- zoo2
                1.  L5 -- numbered1
                2.  L5 -- numbered2
            -   L4 -- zoo1
            -   L4 -- zoo2
        -   L3 -- baz1
        -   L3 -- baz2
    -   L2 -- bar1
    -   L2 -- bar2
-   L1 -- foo1
-   L1 -- foo2


## Unordered list inside descriptive list {#unordered-list-inside-descriptive-list}

bar1
: description for bar1
    -   foo1
    -   foo2

bar2
: description for bar2
    -   foo3
    -   foo4


## Descriptive list inside unordered list {#descriptive-list-inside-unordered-list}

**Seems like Blackfriday style descriptive list syntax does not work
when that list is nested in other lists.**

So in that case, switch back to the descriptive list syntax used in
`ox-md`.

---

-   foo1
    -   **bar1:** description for bar1
    -   **bar2:** description for bar2
-   foo2
    -   **bar3:** description for bar3
    -   **bar4:** description for bar4
