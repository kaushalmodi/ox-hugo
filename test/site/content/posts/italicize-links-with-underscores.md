+++
title = "Italicize links with underscores"
description = "Test that links with underscores can be italicized/emboldened/both."
tags = ["body", "italic", "links", "underscore"]
draft = false
+++

`ox-hugo` Issue #[170](https://github.com/kaushalmodi/ox-hugo/issues/170)


## External links {#external-links}

Italic
: _What is a [two's complement](https://en.wikipedia.org/wiki/Two%27s%5Fcomplement)?_

Bold
: **What is a [two's complement](https://en.wikipedia.org/wiki/Two%27s%5Fcomplement)?**

Bold + Italic
: _**What is a [two's complement](https://en.wikipedia.org/wiki/Two%27s%5Fcomplement)?**_


## Internal links {#internal-links}

Italic
: _Link to [another post on this site]({{< relref "post_with_underscore_in_name" >}})?_

Bold
: **Link to [another post on this site]({{< relref "post_with_underscore_in_name" >}})?**

Bold + Italic
: _**Link to [another post on this site]({{< relref "post_with_underscore_in_name" >}})?**_
