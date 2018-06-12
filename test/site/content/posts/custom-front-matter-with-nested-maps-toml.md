+++
title = "Custom front matter with nested maps in TOML"
description = "Custom TOML front-matter with TOML tables."
tags = ["custom-fm", "TOML-Table", "nested-map"]
draft = false
[dog]
  legs = 4
  eyes = 2
  friends = ["poo", "boo"]
[header]
  image = "projects/Readingabook.jpg"
  caption = "stay hungry, stay foolish"
[collection]
  animals = ["dog", "cat", "penguin", "mountain gorilla"]
  nothing = false
  nonnil = true
  strings-symbols = ["abc", "def", "two words"]
  integers = [123, -5, 17, 1_234]
  floats = [12.3, -5.0, -1.7e-05]
  booleans = [true, false]
[random]
  foo = "bar"
[empty_string_value]
  empty = ""
+++

`ox-hugo` Issue #[139](https://github.com/kaushalmodi/ox-hugo/issues/139)
