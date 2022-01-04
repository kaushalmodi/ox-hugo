+++
title = "Italicize links with underscores (Goldmark)"
description = """
  Test that links with underscores can be italicized/emboldened/both
  without replacing underscores with `%5f`.
  """
aliases = ["/posts/italicize-links-with-underscores"]
tags = ["body", "italic", "links", "underscore", "goldmark"]
draft = false
+++

`ox-hugo` Issue #[308](https://github.com/kaushalmodi/ox-hugo/issues/308), `ox-hugo` Issue #[170](https://github.com/kaushalmodi/ox-hugo/issues/170)


## External links {#external-links}

Italic
: _What is a [two's complement](https://en.wikipedia.org/wiki/Two's_complement)?_

Bold
: **What is a [two's complement](https://en.wikipedia.org/wiki/Two's_complement)?**

Bold + Italic
: _**What is a [two's complement](https://en.wikipedia.org/wiki/Two's_complement)?**_


## Internal links {#internal-links}

Italic
: _Link to [another post on this site]({{< relref "post_with_underscore_in_name" >}})?_

Bold
: **Link to [another post on this site]({{< relref "post_with_underscore_in_name" >}})?**

Bold + Italic
: _**Link to [another post on this site]({{< relref "post_with_underscore_in_name" >}})?**_
