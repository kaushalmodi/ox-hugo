---
title: "Extra Front-matter (YAML, TOML extra)"
tags: ["front-matter", "extra", "verbatim", "src-block", "yaml", "toml-extra"]
draft: false
weight: 5
widget: "about"
active: true
interests:
  interests: ["Artificial Intelligence", "Computational Linguistics", "Information Retrieval"]
---

`ox-hugo` Issue #[226](https://github.com/kaushalmodi/ox-hugo/issues/226)

The `#+begin_src toml :front_matter_extra t` TOML block here will get
discarded because the front-matter type is YAML.

```toml
# this TOML block will export to the Markdown body
```
