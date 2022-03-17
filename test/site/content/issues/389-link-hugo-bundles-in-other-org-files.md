+++
title = "Issue 389 – Link to Hugo bundle subtrees in other Org files"
description = "Test the linking of Hugo bundle subtrees in other Org files."
tags = ["issues", "cross-linking", "bundle"]
draft = false
+++

`ox-hugo` Issue #[389](https://github.com/kaushalmodi/ox-hugo/issues/389)

-   [Link to Page Bundle A]({{< relref "page-bundle-a" >}})
-   [Link to Page Bundle B]({{< relref "page-bundle-b" >}})
-   [Link to Branch Bundle C]({{< relref "branch-bundle-c" >}})

<!--listend-->

-   [Link to a regular post subtree]({{< relref "planning-info" >}}) -- Here, the `EXPORT_FILE_NAME` is
    not a direct _sluggified_ version of the subtree heading.
