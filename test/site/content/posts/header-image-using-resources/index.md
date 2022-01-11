+++
title = "Header image using Resources"
author = ["Eastern Zhang"]
description = "Using the Hugo Resources feature for post header images."
date = 2018-03-01T00:10:00+08:00
tags = ["resources", "toml", "header", "image"]
draft = false
[[resources]]
  src = "stay_hungry*.jpg"
  title = "Stay Hungry Stay Foolish"
  [resources.params]
    credit = "quotefancy.com"
    caption = "Stay Hungry Stay Foolish"
    url = "https://quotefancy.com/quote/13609/Steve-Jobs-Stay-hungry-Stay-foolish"
+++

This test suggests an alternative way to do what the OP wants in
`ox-hugo` Issue #[139](https://github.com/kaushalmodi/ox-hugo/issues/139).

Instead of having a TOML front-matter like:

```toml
+++
title = "fourth test"
author = ["Eason Zhang"]
date = 2018-03-01T00:10:00+08:00
lastmod = 2018-03-01T00:18:00+08:00
tags = ["project"]
draft = true
summary = "summary"
image_preview = "projects/bubbles.jpg"
[header]
  image = "projects/Readingabook.jpg"
  caption = "stay hungry, stay foolish"
+++
```

this post shows an example of doing something similar by exporting the
post as a **Page Bundle** and using the **Resources** feature. Here's how
the resource config and other front-matter is set in Org:

```org
:PROPERTIES:
:EXPORT_HUGO_BUNDLE: header-image-using-resources
:EXPORT_FILE_NAME: index
:EXPORT_AUTHOR: Eastern Zhang
:EXPORT_DATE: 2018-03-01T00:10:00+08:00
:EXPORT_HUGO_RESOURCES: :src "stay_hungry*.jpg" :title "Stay Hungry Stay Foolish"
:EXPORT_HUGO_RESOURCES+: :caption "Stay Hungry Stay Foolish"
:EXPORT_HUGO_RESOURCES+: :credit "quotefancy.com"
:EXPORT_HUGO_RESOURCES+: :url "https://quotefancy.com/quote/13609/Steve-Jobs-Stay-hungry-Stay-foolish"
:END:
```
