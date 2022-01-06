+++
title = "Figures from included Org file"
tags = ["image", "include"]
draft = false
+++

`ox-hugo` Issue #[339](https://github.com/kaushalmodi/ox-hugo/issues/339)


## Including another subtree from this same file {#including-another-subtree-from-this-same-file}

This test can be truly tested by locally deleting
`test/site/static/ox-hugo/copy-2-of-unicorn-logo.png` from this repo,
exporting this file and checking that the file got copied again.

This is an inline non-hyperlinked image without alt text or other
attributes:
[![](/ox-hugo/copy-2-of-unicorn-logo.png)](/ox-hugo/copy-2-of-unicorn-logo.png).


## Including a subtree from a file in different directory {#including-a-subtree-from-a-file-in-different-directory}

This test can be truly tested by locally deleting
`test/site/static/ox-hugo/gnu.png` from this repo, exporting this file
and checking that the file got copied again.

Post 1 contents

I have the `gnu.png` file in the same directory that contains the
source Org file of this post.

```text
./content-org
├── images-in-content/
│  ├── images-in-content.org
│  │    * Post 1 (Org subtree)   →  ./content/images-in-content/post1.md  **THIS POST**
│  │    * Post 2 (Org subtree)   →  ./content/images-in-content/post2.md
│  ├── gnu.png                   →  ./static/ox-hugo/gnu.png              **BELOW IMAGE**
│  └── org.png                   →  ./static/ox-hugo/org.png
├── post3/
│  ├── post3.org                 →  ./content/images-in-content/post3.md
│  └── gnu-copy.png              →  ./static/ox-hugo/gnu-copy.png
└── post4/
   ├── post4.org                 →  ./content/images-in-content/post4.md
   └── org-copy.png              →  ./static/ox-hugo/org-copy.png
```

{{< figure src="/ox-hugo/gnu.png" >}}
