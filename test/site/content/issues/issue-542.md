+++
title = "Issue # 542"
description = "Test links to headings whose anchors are derived from org-id's."
tags = ["org-id", "link", "anchor", "heading"]
draft = false
+++

`ox-hugo` Issue #[542](https://github.com/kaushalmodi/ox-hugo/issues/542)

-   [Link]({{< relref "issue-556.md#heading-abc" >}}) to a heading that has both CUSTOM_ID and ID set.
-   [Link]({{< relref "issue-556.md#909536ed-b636-4bb9-9cc6-6a06992d8853" >}}) to a heading that has only the ID set.

<!--listend-->

-   [Link]({{< relref "issue-556#heading-1" >}}) to a heading without CUSTOM_ID or ID properties.
-   [Link]({{< relref "issue-556#heading-xyz" >}}) to a heading with only the CUSTOM_ID property set.
-   [Link]({{< relref "issue-556#909536ed-b636-4bb9-9cc6-6a06992d8853" >}}) to a heading with only the ID property set.
-   [Link]({{< relref "issue-556#heading-abc" >}}) to a heading with both CUSTOM_ID and ID properties set.

<!--listend-->

-   [Link]({{< relref "issue-556#heading-abc" >}}) to a heading using CUSTOM_ID where the heading has both
    CUSTOM_ID and ID properties set.
-   [Link]({{< relref "issue-556#heading-xyz" >}}) to a heading using CUSTOM_ID where the heading has only the
    CUSTOM_ID property set.
