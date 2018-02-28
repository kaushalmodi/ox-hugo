---
title: "Custom front matter with nested maps in YAML"
description: "Custom YAML front-matter with nested maps."
tags: ["custom-fm", "TOML-Table", "nested-map"]
draft: false
dog:
  legs: 4
  eyes: 2
  friends: ["poo", "boo"]
header:
  image: "projects/Readingabook.jpg"
  caption: "stay hungry, stay foolish"
collection:
  nothing: false
  nonnil: true
  animals: ["dog", "cat", "penguin", "mountain gorilla"]
  strings-symbols: ["abc", "def", "two words"]
  integers: [123, -5, 17, 1_234]
  floats: [12.3, -5.0, -1.7e-05]
  booleans: [true, false]
---

`ox-hugo` Issue #[139](https://github.com/kaushalmodi/ox-hugo/issues/139)
