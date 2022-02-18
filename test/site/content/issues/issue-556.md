+++
title = "Issue # 556"
description = """
  Test the `org-hugo-get-custom-id` `org-hugo-get-id
  org-hugo-get-heading-slug` `org-hugo-get-md5` `precedence set in
  ~org-hugo-anchor-functions`.
  """
tags = ["org-id", "anchor", "heading"]
draft = false
+++

`ox-hugo` Issue #[556](https://github.com/kaushalmodi/ox-hugo/issues/556)


## Heading 1 {#heading-1}

This heading's anchor will be derived off the heading string.


### Heading 1.1 {#48e6dfd4-93d9-4811-855e-c739470e83d1}

This heading's anchor will be derived off the `ID`.


## Heading 2 {#heading-xyz}

This heading's anchor will be derived off the `CUSTOM_ID`.


## Heading 3 {#heading-abc}

This heading's anchor will be derived off the `CUSTOM_ID` as that
takes precedence over the `ID`.


### Heading 3.1 {#909536ed-b636-4bb9-9cc6-6a06992d8853}

This heading's anchor will be derived off the `ID`.


## Heading 4 {#6bc923a1-3543-440b-ace3-17c049cbbe0a}

This heading's anchor will be derived off the `ID`.


## % {#0bcef9}

This heading's anchor will be derived from _md5_ of the title as it is
not alphanumeric.


## Org Target {#org-target}

paragraph one

<span class="org-target" id="org-target--paragraph-2"></span>
paragraph two

<span class="org-target" id="paragraph-3"></span>
paragraph three
