+++
title = "Post with resources in front-matter (TOML)"
tags = ["resources", "toml"]
draft = false
[[resources]]
  src = "image-4.png"
  title = "TOML: The Fourth Image"
[[resources]]
  src = "*.png"
  name = "my-cool-image-:counter"
  title = "TOML: The Image #:counter"
  [resources.params]
    byline = "bep"
[[resources]]
  src = "*.jpg"
  title = "JPEG Image #:counter"
+++
