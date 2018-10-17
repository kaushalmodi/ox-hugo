---
title: "Extra Front-matter (YAML, YAML extra)"
tags: ["front-matter", "extra", "verbatim", "src-block", "yaml", "yaml-extra"]
draft: false
weight: 5
widget: "about"
active: true
interests:
  interests: ["Artificial Intelligence", "Computational Linguistics", "Information Retrieval"]
education:
  courses:
    - course: PhD in Artificial Intelligence
      institution: Stanford University
      year: 2012
    - course: MEng in Artificial Intelligence
      institution: Massachusetts Institute of Technology
      year: 2009
    - course: BSc in Artificial Intelligence
      institution: Massachusetts Institute of Technology
      year: 2008
---

`ox-hugo` Issue #[226](https://github.com/kaushalmodi/ox-hugo/issues/226)

The `#+begin_src yaml :front_matter_extra t` YAML block here will get
appended to the YAML front-matter.

```toml
# this TOML block will export to the Markdown body
```
