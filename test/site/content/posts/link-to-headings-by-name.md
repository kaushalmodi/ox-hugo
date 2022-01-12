+++
title = "Link to headings by name"
tags = ["links", "internal-links", "toc", "headings", "export-option"]
draft = false
+++

<style>
  .ox-hugo-toc ul {
    list-style: none;
  }
</style>
<div class="ox-hugo-toc toc">

<div class="heading">Table of Contents</div>

- [Alpha 101](#alpha-101)
- <span class="section-num">1</span> [Beta 102](#beta-102)
    - <span class="section-num">1.1</span> [Gamma 102.1](#gamma-102-dot-1)
        - <span class="section-num">1.1.1</span> [Delta 102.1.1](#delta-102-dot-1-dot-1)
        - <span class="section-num">1.1.2</span> [Epsilon 102.1.2](#epsilon-102-dot-1-dot-2)
- [Zeta 103 which has **some** _markup_](#zeta-103-which-has-some-markup)
    - [Links (no descriptions) to headings with section numbers](#links--no-descriptions--to-headings-with-section-numbers)

</div>
<!--endtoc-->


## Alpha 101 {#alpha-101}

-   Link (with description) to a heading with section number: [Link to
    _Beta 102_ heading](#beta-102)
-   Link (no description) to a heading without section number: [Zeta 103 which has **some** _markup_](#zeta-103-which-has-some-markup).

    The space after that `*` in the link is optional.. so this also
    works: [Zeta 103 which has **some** _markup_](#zeta-103-which-has-some-markup).


## <span class="section-num">1</span> Beta 102 {#beta-102}

-   Link (with description) to a heading without section number: [Link to
    _Alpha 101_ heading](#alpha-101)


### <span class="section-num">1.1</span> Gamma 102.1 {#gamma-102-dot-1}


#### <span class="section-num">1.1.1</span> Delta 102.1.1 {#delta-102-dot-1-dot-1}


#### <span class="section-num">1.1.2</span> Epsilon 102.1.2 {#epsilon-102-dot-1-dot-2}


## Zeta 103 which has **some** _markup_ {#zeta-103-which-has-some-markup}


### Links (no descriptions) to headings with section numbers {#links--no-descriptions--to-headings-with-section-numbers}

-   Section [1.1](#gamma-102-dot-1)
-   Section [1.1.1](#delta-102-dot-1-dot-1)
-   Section [1.1.2](#epsilon-102-dot-1-dot-2)
