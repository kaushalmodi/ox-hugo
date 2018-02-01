+++
title = "Post 3"
draft = false
+++

Post 3 contents

I have the `gnu-copy.png` file in the same directory that contains the
source Org file of this post.

```text
./content-org
├── images-in-content/
│  ├── images-in-content.org
│  │    * Post 1 (Org subtree)   →  ./content/images-in-content/post1.md
│  │    * Post 2 (Org subtree)   →  ./content/images-in-content/post2.md
│  ├── gnu.png                   →  ./static/ox-hugo/gnu.png
│  └── org.png                   →  ./static/ox-hugo/org.png
├── post3/
│  ├── post3.org                 →  ./content/images-in-content/post3.md  **THIS POST**
│  └── gnu-copy.png              →  ./static/ox-hugo/gnu-copy.png         **BELOW IMAGE**
└── post4/
   ├── post4.org                 →  ./content/images-in-content/post4.md
   └── org-copy.png              →  ./static/ox-hugo/org-copy.png
```

{{< figure src="/ox-hugo/gnu-copy.png" >}}
