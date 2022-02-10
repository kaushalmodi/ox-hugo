---
title: "Citations Example (YAML)"
description: >
  Test the parsing of Pandoc Citations, while also testing that ox-hugo
  exported Markdown doesn't get broken -- YAML front-matter.
tags: ["citations", "pandoc", "yaml"]
draft: false
---

`ox-hugo` Issue
\#[175](https://github.com/kaushalmodi/ox-hugo/issues/175)

## Section 1

Here is a test example file with an in-text citation where someone
important says something important (e.g. Loncar (2016)). And here is
another bit of blah with a footnote citation.[^fn:1]

See [Section 2](#citation-example-yaml-section-2).

## Section 2 {#citation-example-yaml-section-2}

Content in section 2.

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

<div id="ref-thompson2016" class="csl-entry">

Thompson, Morgan, Toni Adleberg, Sam Sims, and Eddy Nahmias. 2016. "Why
Do Women Leave Philosophy? Surveying Students at the Introductory
Level."

</div>

</div>

[^fn:1]: See (Thompson et al. 2016).
