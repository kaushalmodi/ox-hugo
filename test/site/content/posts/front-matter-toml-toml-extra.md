+++
title = "Extra Front-matter (TOML, TOML extra)"
tags = ["front-matter", "extra", "verbatim", "src-block", "toml", "toml-extra"]
draft = false
weight = 5
widget = "about"
active = true
[interests]
  interests = ["Artificial Intelligence", "Computational Linguistics", "Information Retrieval"]
# List your qualifications (such as academic degrees).
[[education.courses]]
  course = "PhD in Artificial Intelligence"
  institution = "Stanford University"
  year = 2012

[[education.courses]]
  course = "MEng in Artificial Intelligence"
  institution = "Massachusetts Institute of Technology"
  year = 2009

[[education.courses]]
  course = "BSc in Artificial Intelligence"
  institution = "Massachusetts Institute of Technology"
  year = 2008
+++

`ox-hugo` Issue #[226](https://github.com/kaushalmodi/ox-hugo/issues/226)

The `#+begin_src toml :front_matter_extra t` TOML block here will get
appended to the TOML front-matter.

```toml
# this TOML block will export to the Markdown body
```
