+++
title = "Extra Front-matter (TOML, YAML extra)"
tags = ["front-matter", "extra", "verbatim", "src-block", "toml", "yaml-extra"]
draft = false
weight = 5
widget = "about"
active = true
[interests]
  interests = ["Artificial Intelligence", "Computational Linguistics", "Information Retrieval"]
+++

`ox-hugo` Issue #[226](https://github.com/kaushalmodi/ox-hugo/issues/226)

The `#+begin_src yaml :front_matter_extra t` YAML block here will get
discarded because the front-matter type is TOML.

```toml
# this TOML block will export to the Markdown body
```
