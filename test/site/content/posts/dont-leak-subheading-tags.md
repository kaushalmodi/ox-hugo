+++
title = "Do not leak post's immediate sub-heading tag into the front-matter"
description = """
  Disable exporting of sub-heading tags by setting export option
  `tags:nil`.
  """
tags = ["expected-tag", "post-heading-followed-soon-with-subheading"]
draft = false
+++

## Sub-heading 1 {#sub-heading-1}

This is a **special** case where:

-   A post has a sub-heading as the first line in its body, and
-   That sub-heading has a tag too!

The passing case for this test would be that the `unexpected_tag` does
not leak into the post's front-matter.
