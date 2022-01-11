+++
title = "Single Post with TOML front matter"
description = "Some description for this post."
date = 2017-07-20
tags = ["single", "toml", "cross-link"]
categories = ["cat1", "cat2"]
draft = false
[menu.foo]
  parent = "main"
  weight = 10
  identifier = "single-toml"
+++

This is a single post. You do not need to set the `EXPORT_FILE_NAME`
property in here. But then you also lose the tag and property
inheritance Org awesomeness.


## First heading in this post {#first-heading-in-this-post}

This is a under first heading.


## Second heading in this post {#second-heading-in-this-post}

This is a under second heading.


## Cross-linking {#cross-linking}

All of the below linked Org files **have to have** exported posts using
the _per-file_ flow, and they **cannot be** page bundles or have set a
different `#+export_file_name` --- See `ox-hugo` Issue #[131](https://github.com/kaushalmodi/ox-hugo/issues/131) for
requirements for this basic cross-linking to work as expected.


### Link to file in the same directory {#link-to-file-in-the-same-directory}

This file will export the front-matter in TOML (default). See
[{{< relref "post-yaml" >}}]({{< relref "post-yaml" >}}) that exports that in YAML.


### Link + Description {#link-plus-description}

Here's the same link with description: [Post exported with YAML
front-matter]({{< relref "post-yaml" >}}).


### Link to file in a different directory {#link-to-file-in-a-different-directory}

[{{< relref "post3" >}}]({{< relref "post3" >}}).


### Link to file that has a Hugo slug set {#link-to-file-that-has-a-hugo-slug-set}

This one is magic! You can link to the Org file, no matter what slug
is set in the exported file.. Hugo will do the job of translating from
the Markdown file name to the actual link/slug.

[{{< relref "post-with-slug" >}}]({{< relref "post-with-slug" >}})


### Link to the CUSTOM_ID of a different file {#link-to-the-custom-id-of-a-different-file}

[{{< relref "post-yaml#first-heading" >}}]({{< relref "post-yaml#first-heading" >}})
