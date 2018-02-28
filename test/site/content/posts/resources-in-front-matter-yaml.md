---
title: "Post with resources in front-matter (YAML)"
tags: ["resources", "yaml"]
draft: false
resources:
- src: "image-4.png"
  title: "The Fourth Image"
- src: "*.png"
  name: "my-cool-image-:counter"
  title: "The Image #:counter"
  params:
    byline: "bep"
- src: "*.jpg"
  title: "JPEG Image #:counter"
---
