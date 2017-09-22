+++
title = "Screenshots"
tags = ["wiki"]
draft = false
[menu.example]
  weight = 3001
  identifier = "screenshots"
+++

## One post per Org subtree (preferred) {#screenshot-one-post-per-subtree}

[{{<figure src="/images/one-post-per-subtree.png">}}](/images/one-post-per-subtree.png)

-   **Files in above screenshot:** [Org](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content-org/screenshot-subtree-export-example.org) -> [Markdown](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content/writing-hugo-blog-in-org-subtree-export.md)


## One post per Org file {#screenshot-one-post-per-file}

[{{<figure src="/images/one-post-per-file.png">}}](/images/one-post-per-file.png)

-   **Files in above screenshot:** [Org](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content-org/writing-hugo-blog-in-org-file-export.org) -> [Markdown](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content/writing-hugo-blog-in-org-file-export.md)


## Editorial {#org-blogging-flow-editorial}

The preferred way to organize the posts is as Org subtrees (also the
main reason to write this package, as nothing like that was out there)
as it makes the meta-data management for Hugo front-matter pretty
effortless.

If you are a _one Org-file per post_ type of a person, that flow works
too! Just note that in this flow many of those `#+HUGO_` properties
need to be managed manually.. just as one would manage the front-matter
in Markdown files --- See the Org versions in the above screenshots for
comparison.
