+++
title = "Keyword Collection"
author = ["firstname1 lastname1", "firstname2 lastname2", "firstname3 lastname3"]
aliases = ["/posts/keyword-concatenation", "/posts/keyword-merging"]
images = ["image 1", "image 2"]
keywords = ["keyword1", "keyword2", "three word keywords3"]
outputs = ["html", "json"]
series = ["series 1", "series 2"]
tags = ["mega front-matter", "keys", "collection", "concatenation", "merging"]
categories = ["cat1", "cat2"]
videos = ["video 1", "video 2"]
draft = false
categories_weight = 999
tags_weight = 88
weight = 7
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
[dog]
  legs = 4
  eyes = 2
  friends = ["poo", "boo"]
[header]
  image = "projects/Readingabook.jpg"
  caption = "stay hungry, stay foolish"
[collection]
  nothing = false
  nonnil = true
  animals = ["dog", "cat", "penguin", "mountain gorilla"]
  strings-symbols = ["abc", "def", "two words"]
  integers = [123, -5, 17, 1_234]
  floats = [12.3, -5.0, -1.7e-05]
  booleans = [true, false]
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

<!--more-->


## Keyword Collection Tested to work <code>[15/15]</code> {#keyword-collection-tested-to-work}

-   [X] `#+author`
-   [X] `#+hugo_tags`
-   [X] `#+hugo_categories`
-   [X] `#+hugo_menu`
-   [X] `#+hugo_menu_override`
-   [X] `#+hugo_custom_front_matter`
-   [X] `#+hugo_front_matter_key_replace`
-   [X] `#+hugo_aliases`
-   [X] `#+keywords`
-   [X] `#+hugo_outputs`
-   [X] `#+hugo_resources`
-   [X] `#+hugo_images`
-   [X] `#+hugo_videos`
-   [X] `#+hugo_series`
-   [X] `#+hugo_weight`
