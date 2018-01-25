+++
title = "Keyword Collection"
author = ["firstname1 lastname1", "firstname2 lastname2", "firstname3 lastname3"]
aliases = ["/posts/keyword-concatenation", "/posts/keyword-merging"]
keywords = ["keyword1", "keyword2", "three word keywords3"]
outputs = ["html", "json"]
tags = ["mega front-matter", "keys", "collection", "concatenation", "merging"]
categories = ["cat1", "cat2"]
draft = false
myfoo = "bar"
mybaz = "zoo"
alpha = 1
beta = "two words"
gamma = 10
animals = ["dog", "cat", "penguin", "mountain gorilla"]
strings-symbols = ["abc", "def", "two words"]
integers = [123, -5, 17, 1_234]
floats = [12.3, -5.0, -1.7e-05]
booleans = [true, false]
[blackfriday]
  extensionsmask = ["fencedCode", "strikethrough"]
  extensions = ["tabSizeEight", "hardLineBreak"]
  plainIDAnchors = false
  hrefTargetBlank = true
  smartDashes = false
  fractions = false
  angledQuotes = true
[menu.foo]
  identifier = "keyword-collection"
  weight = 10
[[resources]]
  src = "*.png"
  name = "my-cool-image-:counter"
  title = "The Image #:counter"
  [resources.params]
    foo = "bar"
    floats = [12.3, -5.0, -1.7e-05]
    strings-symbols = ["abc", "def", "two words"]
    animals = ["dog", "cat", "penguin", "mountain gorilla"]
    integers = [123, -5, 17, 1_234]
    booleans = [true, false]
    byline = "bep"
[[resources]]
  src = "image-4.png"
  title = "The Fourth Image"
[[resources]]
  src = "*.jpg"
  title = "JPEG Image #:counter"
+++

This is a test post that tests that keywords set across multiple Org
keywords get collected.


## Keyword Collection Tested to work <code>[12/12]</code> {#keyword-collection-tested-to-work}

-   [X] `#+AUTHOR`
-   [X] `#+HUGO_TAGS`
-   [X] `#+HUGO_CATEGORIES`
-   [X] `#+HUGO_MENU`
-   [X] `#+HUGO_MENU_OVERRIDE`
-   [X] `#+HUGO_CUSTOM_FRONT_MATTER`
-   [X] `#+HUGO_BLACKFRIDAY`
-   [X] `#+HUGO_FRONT_MATTER_KEY_REPLACE`
-   [X] `#+HUGO_ALIASES`
-   [X] `#+KEYWORDS`
-   [X] `#+HUGO_OUTPUTS`
-   [X] `#+HUGO_RESOURCES`
