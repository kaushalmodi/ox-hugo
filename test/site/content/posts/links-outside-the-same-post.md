+++
title = "Links outside the same post"
tags = ["links"]
draft = false
+++

`ox-hugo` Issue #[30](https://github.com/kaushalmodi/ox-hugo/issues/30)


## External links with search options <span class="tag"><span class="external_links">external-links</span></span> {#external-links-with-search-options}

Links between documents can contain some search options. Only links
to a heading with a **:CUSTOM_ID** property will be resolved to the
appropriate location in the linked file. Links to headings and
links to targets will be resolved to the containing file.

-   [Link to CUSTOM_ID]({{< relref "link-destination#external-target" >}})
-   [Link to a heading]({{< relref "link-destination" >}})
-   [Link to a target]({{< relref "link-destination" >}})


## Internal links <span class="tag"><span class="internal_links">internal-links</span></span> {#internal-links}

Internal links point to targets in the current subtree that will be
exported to the same Hugo post as the link source. To handle links to
an **:ID** property, the `org-id` feature must first be loaded, either
through `org-customize` or by adding `(require 'org-id)` in your Emacs
init file.

-   [Link to CUSTOM_ID within the same post](#internal-target)
-   [Link to ID within the same post](#internal-target)
-   [Link to heading within the same post](#internal-target)
-   [Link to target within the same post](#org-target--internal-target)


## Cross-post links <span class="tag"><span class="crosspost_links">crosspost-links</span></span> {#cross-post-links}

Cross-post links are internal links pointing to targets in a different
subtree that will be exported to another Hugo post than the link
source in subtree-based exports. The Hugo's `ref` and `relref`
shortcodes only supports anchors to headings, so links to a heading,
a **:CUSTOM_ID** property, or an **:ID** property will be resolved to the
appropriate location in the linked file, but links to targets will be
resolved to the containing post.


### Links without descriptions {#links-without-descriptions}

-   Link to CUSTOM_ID outside the same post: [External target]({{< relref "link-destination#external-target" >}})
-   Link to ID outside the same post: [External target]({{< relref "link-destination#external-target" >}})
-   Link to target outside the same post: [External target]({{< relref "link-destination#external-target" >}})
-   Another link to target outside the same post: [External target with **bold** and _italic_]({{< relref "link-destination#external-target-with-bold-and-italic" >}})
-   Link to subtree by CUSTOM_ID: [Link destination]({{< relref "link-destination" >}})
-   Link to subtree by ID: [Link destination]({{< relref "link-destination" >}})
-   Link to subtree by heading: [Link destination]({{< relref "link-destination" >}})


### Links with descriptions {#links-with-descriptions}

-   [Link to CUSTOM_ID outside the same post]({{< relref "link-destination#external-target" >}})
-   [Link to ID outside the same post]({{< relref "link-destination#external-target" >}})
-   [Link to target outside the same post]({{< relref "link-destination#external-target" >}})
-   [Another link to target outside the same post]({{< relref "link-destination#external-target-with-bold-and-italic" >}})
-   [Link to subtree by CUSTOM_ID]({{< relref "link-destination" >}})
-   [Link to subtree by ID]({{< relref "link-destination" >}})
-   [Link to subtree by heading]({{< relref "link-destination" >}})


## Internal target {#internal-target}

<span class="org-target" id="org-target--internal-target"></span>


## Link destination {#link-destination}


### External target {#external-target}

<span class="org-target" id="org-target--external-target"></span>


### External target with **bold** and _italic_ {#external-target-with-bold-and-italic}
