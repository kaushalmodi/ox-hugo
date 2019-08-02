+++
title = "Links outside the same post"
tags = ["links"]
draft = false
+++

`ox-hugo` Issue #[30](https://github.com/kaushalmodi/ox-hugo/issues/30)


## External links with search options {#external-links-with-search-options}

Links between documents can contain some search options. Only links
to a heading with a **:CUSTOM\_ID** property will be resolved to the
appropriate location in the linked file. Links to headlines and
links to targets will be resolved to the containing file.

-   [Link to CUSTOM\_ID]({{< relref "link-destination#external-target" >}})
-   [Link to a headline]({{< relref "link-destination" >}})
-   [Link to a target]({{< relref "link-destination" >}})


## Internal links {#internal-links}

Internal links point to targets in the current subtree that will be
exported to the same Hugo post as the link source. To handle links to
an **:ID** property, the `org-id` feature must first be loaded, either
through `org-customize` or by adding `(require 'org-id)` in your Emacs
init file.

-   [Link to CUSTOM\_ID within the same post](#internal-target)
-   [Link to ID within the same post](#internal-target)
-   [Link to headline within the same post](#internal-target)
-   [Link to target within the same post](#org7d2d8b5)


## Cross-post links {#cross-post-links}

Cross-post links are internal links pointing to targets in a different
subtree that will be exported to another Hugo post than the link
source in subtree-based exports. The Hugo's `ref` and `relref`
shortcodes only supports anchors to headlines, so links to a heading,
a **:CUSTOM\_ID** property, or an **:ID** property will be resolved to the
appropriate location in the linked file, but links to targets will be
resolved to the containing post.


### Links without descriptions {#links-without-descriptions}

-   Link to CUSTOM\_ID outside the same post: [External target]({{< relref "link-destination#external-target" >}})
-   Link to ID outside the same post: [External target]({{< relref "link-destination#external-target" >}})
-   Link to target outside the same post: [External target]({{< relref "link-destination#external-target" >}})
-   Another link to target outside the same post: [External target with **bold** and _italic_]({{< relref "link-destination#external-target-with-bold-and-italic" >}})
-   Link to subtree by CUSTOM\_ID: [Link destination]({{< relref "link-destination" >}})
-   Link to subtree by ID: [Link destination]({{< relref "link-destination" >}})
-   Link to subtree by headline: [Link destination]({{< relref "link-destination" >}})


### Links with descriptions {#links-with-descriptions}

-   [Link to CUSTOM\_ID outside the same post]({{< relref "link-destination#external-target" >}})
-   [Link to ID outside the same post]({{< relref "link-destination#external-target" >}})
-   [Link to target outside the same post]({{< relref "link-destination#external-target" >}})
-   [Another link to target outside the same post]({{< relref "link-destination#external-target-with-bold-and-italic" >}})
-   [Link to subtree by CUSTOM\_ID]({{< relref "link-destination" >}})
-   [Link to subtree by ID]({{< relref "link-destination" >}})
-   [Link to subtree by headline]({{< relref "link-destination" >}})


## Internal target {#internal-target}

<a id="org7d2d8b5"></a>


## Link destination {#link-destination}


### External target {#external-target}

<a id="orga9009ef"></a>


### External target with **bold** and _italic_ {#external-target-with-bold-and-italic}
