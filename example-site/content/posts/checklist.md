+++
title = "Checklist"
date = 2017-08-02
tags = ["lists"]
draft = false
+++

This is a check-list:


## Checklist <code>[40%]</code> {#checklist-code-40-code}

Checklist showing progress in percentage.

-   [ ] Task 1
-   [ ] Task 2
-   [X] Task 3
-   [ ] Task 4
-   [X] Task 5


## Checklist <code>[2/5]</code> {#checklist-code-2-5-code}

Checklist showing progress in ratio.

-   [ ] Task 1
-   [ ] Task 2
-   [X] Task 3
-   [ ] Task 4
-   [X] Task 5


### Needs Blackfriday front matter {#needs-blackfriday-front-matter}

The "2/5" ratio is rendered as a fraction by Blackfriday. Need to
implement feature in [Issue # 37](https://github.com/kaushalmodi/ox-hugo/issues/37) so that the fraction rendering can be
disabled in the front matter (setting `[blackfriday] fractions` to
`false` [[Ref](https://gohugo.io/getting-started/configuration/#configure-blackfriday)]).
