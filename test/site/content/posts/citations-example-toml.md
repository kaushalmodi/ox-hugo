+++
title = "Citations Example (TOML)"
description = """
  Test the parsing of Pandoc Citations, while also testing that ox-hugo
  exported Markdown doesn't get broken -- TOML front-matter.
  """
tags = ["citations", "pandoc", "toml"]
draft = false
+++

`ox-hugo` Issue
\#[175](https://github.com/kaushalmodi/ox-hugo/issues/175)

## Section 1

Here is a test example file with an in-text citation where someone
important says something important (e.g. Loncar (2016)). And here is
another bit of blah with a footnote citation.[^fn:1]

See [Section 2](#citation-example-toml-section-2).

## Section 2 {#citation-example-toml-section-2}

Content in section 2.

## Testing random Hugo shortcodes

{{% mdshortcode %}} Text containing **Markdown** {{% /mdshortcode %}}

Some text.

{{< myshortcode >}} Text not containing **Markdown** {{< /myshortcode >}}

This link will generate a `relref` shortcode: Here's a link to an
arbitrarily picked post: [Citation Linking]({{< relref "citation-linking" >}}).

## Testing ox-hugo inserted HTML div tags

<div class="foo">

**bold** *italics*

</div>

## Testing tables

| Header 1 | Header 2 | Header 3 |
|----------|----------|----------|
| a        | b        | c        |
| d        | e        | f        |

## Testing fenced code blocks

``` emacs-lisp
(message "Hello World")
```

## Lists Galore

-   item1 in list
-   item2 in list. The following list is in a separate list body.

<!--listend-->

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

## Citation key with underscore

Ox-hugo manual (Modi, Kaushal (2018))

## References {#references}

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-eilan2016" class="csl-entry">

Eilan, Naomi. 2016. "You Me and the World." *Analysis* 76 (3): 311--24.

</div>

<div id="ref-giovanelli2016" class="csl-entry">

Giovanelli, Marco. 2016. "\"\...But I Still Can't Get Rid of a Sense of
Artificiality\" The Reichenbach--Einstein Debate on the Geometrization
of the Electromagnetic Field." *Studies in History and Philosophy of
Science* 54: 35--51.

</div>

<div id="ref-loncar2016" class="csl-entry">

Loncar, Samuel. 2016. "Why Listen to Philosophers? A Constructive
Critique of Disciplinary Philosophy." *Metaphilosophy* 47 (1): 3--25.

</div>

<div id="ref-ox_hugo_man" class="csl-entry">

Modi, Kaushal. 2018. *Ox-Hugo: A Carefully Crafted Org Exporter Back-End
for Hugo*. North Carolina, USA. <https://ox-hugo.scripter.co/>.

</div>

<div id="ref-thompson2016" class="csl-entry">

Thompson, Morgan, Toni Adleberg, Sam Sims, and Eddy Nahmias. 2016. "Why
Do Women Leave Philosophy? Surveying Students at the Introductory
Level."

</div>

</div>

[^fn:1]: See (Thompson et al. 2016).
