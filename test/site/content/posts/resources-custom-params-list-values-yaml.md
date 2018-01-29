---
title: "Custom resource params with list values"
tags: ["resources", "yaml"]
draft: false
resources:
- src: "*.png"
  params:
    booleans: [true, false]
    integers: [123, -5, 17, 1_234]
    animals: ["dog", "cat", "penguin", "mountain gorilla"]
    strings-symbols: ["abc", "def", "two words"]
    floats: [12.3, -5.0, -1.7e-05]
    foo: "bar"
---
