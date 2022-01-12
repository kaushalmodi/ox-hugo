+++
title = "TOC Local"
description = """
  Test the `#+toc: headlines N local` syntax where a TOC is exported
  containing headings only up to level-N relative to the heading in
  which that keyword is used.
  """
tags = ["keyword", "toc", "local"]
draft = false
+++

Below, TOC is exported with only level-1 headings in this post.

<div class="ox-hugo-toc toc">

<div class="heading">Table of Contents</div>

- [Post sub-heading 1](#post-sub-heading-1)
- [Post sub-heading 2](#post-sub-heading-2)
- [Post sub-heading 3](#post-sub-heading-3)

</div>
<!--endtoc-->

Below exported TOC should look the same as above even when it's
generated using the `local` param as it is at the root level of this
post.

<div class="ox-hugo-toc toc local">

- [Post sub-heading 1](#post-sub-heading-1)
- [Post sub-heading 2](#post-sub-heading-2)
- [Post sub-heading 3](#post-sub-heading-3)

</div>
<!--endtoc-->


## Post sub-heading 1 {#post-sub-heading-1}

Below, TOC is exported with only level-1 headings **relative to** this
"Post sub-heading 1" section.

<div class="ox-hugo-toc toc local">

- [Post sub-heading 1.1](#post-sub-heading-1-dot-1)
- [Post sub-heading 1.2](#post-sub-heading-1-dot-2)
- [Post sub-heading 1.3](#post-sub-heading-1-dot-3)

</div>
<!--endtoc-->


### Post sub-heading 1.1 {#post-sub-heading-1-dot-1}


#### Post sub-heading 1.1.1 {#post-sub-heading-1-dot-1-dot-1}


### Post sub-heading 1.2 {#post-sub-heading-1-dot-2}


### Post sub-heading 1.3 {#post-sub-heading-1-dot-3}


## Post sub-heading 2 {#post-sub-heading-2}


### Post sub-heading 2.1 {#post-sub-heading-2-dot-1}


### Post sub-heading 2.2 {#post-sub-heading-2-dot-2}

Below, TOC is exported with only up to level-2 headings **relative to**
this "Post sub-heading 2.2" section.

<div class="ox-hugo-toc toc local">

- [Post sub-heading 2.2.1](#post-sub-heading-2-dot-2-dot-1)
- [Post sub-heading 2.2.2](#post-sub-heading-2-dot-2-dot-2)
- [Post sub-heading 2.2.3](#post-sub-heading-2-dot-2-dot-3)
    - [Post sub-heading 2.2.3.1](#post-sub-heading-2-dot-2-dot-3-dot-1)
    - [Post sub-heading 2.2.3.2](#post-sub-heading-2-dot-2-dot-3-dot-2)

</div>
<!--endtoc-->


#### Post sub-heading 2.2.1 {#post-sub-heading-2-dot-2-dot-1}


#### Post sub-heading 2.2.2 {#post-sub-heading-2-dot-2-dot-2}


#### Post sub-heading 2.2.3 {#post-sub-heading-2-dot-2-dot-3}

Below, TOC is exported with only level-1 headings **relative to** this
"Post sub-heading 2.2.3" section.

<div class="ox-hugo-toc toc local">

- [Post sub-heading 2.2.3.1](#post-sub-heading-2-dot-2-dot-3-dot-1)
- [Post sub-heading 2.2.3.2](#post-sub-heading-2-dot-2-dot-3-dot-2)

</div>
<!--endtoc-->


##### Post sub-heading 2.2.3.1 {#post-sub-heading-2-dot-2-dot-3-dot-1}


##### Post sub-heading 2.2.3.2 {#post-sub-heading-2-dot-2-dot-3-dot-2}


## Post sub-heading 3 {#post-sub-heading-3}


### Post sub-heading 3.1 {#post-sub-heading-3-dot-1}
