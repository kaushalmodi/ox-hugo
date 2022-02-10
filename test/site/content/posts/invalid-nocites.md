+++
title = "Invalid Nocites"
tags = ["citations", "pandoc", "invalid", "nocite"]
draft = false
+++

This post has citations `@foo` and `@bar` listed in `nocite`
meta-data. But they are invalid as they don't exist in any of the
bibliography files.

But that generates neither a Pandoc warning nor error.

As the final Pandoc output Markdown ends up with **no** references, the
Pandoc output is discarded, and the original `ox-hugo` output is used
instead.
