+++
title = "Post with TOC export option set to 2"
tags = ["toc", "export-option"]
draft = false
+++

## Table of Contents

1.  [Post sub-heading 1](#post-sub-heading-1)
    1.  [Post sub-heading 1.1](#post-sub-heading-1-dot-1)
    2.  [Post sub-heading 1.2](#post-sub-heading-1-dot-2)
    3.  [Post sub-heading 1.3](#post-sub-heading-1-dot-3)
2.  [Post sub-heading 2](#post-sub-heading-2)
    1.  [Post sub-heading 2.1](#post-sub-heading-2-dot-1)
    2.  [Post sub-heading 2.2](#post-sub-heading-2-dot-2)
3.  [Post sub-heading 3](#post-sub-heading-3)
    1.  [Post sub-heading 3.1](#post-sub-heading-3-dot-1)


## Post sub-heading 1 {#post-sub-heading-1}


### Post sub-heading 1.1 {#post-sub-heading-1-dot-1}


#### Post sub-heading 1.1.1 {#post-sub-heading-1-dot-1-dot-1}


### Post sub-heading 1.2 {#post-sub-heading-1-dot-2}


### Post sub-heading 1.3 {#post-sub-heading-1-dot-3}


## Post sub-heading 2 {#post-sub-heading-2}


### Post sub-heading 2.1 {#post-sub-heading-2-dot-1}


### Post sub-heading 2.2 {#post-sub-heading-2-dot-2}


#### Post sub-heading 2.2.1 {#post-sub-heading-2-dot-2-dot-1}


#### Post sub-heading 2.2.2 {#post-sub-heading-2-dot-2-dot-2}

The `UNNUMBERED` property for this subtree is set to `t`. So even when
the TOC export option is set to `t` or a number ≥ 3, this title will
**not** show up in the TOC. That's because the TOC is exported as a
Markdown _ordered list_, and you cannot have ordered list items
without a number prefix!


#### Post sub-heading 2.2.3 {#post-sub-heading-2-dot-2-dot-3}


## Post sub-heading 3 {#post-sub-heading-3}


### Post sub-heading 3.1 {#post-sub-heading-3-dot-1}
