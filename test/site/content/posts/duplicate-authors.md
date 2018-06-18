+++
title = "Uniquify duplicate authors"
author = ["Foo Bar"]
description = "Test that duplicate authors get uniquified in the export."
tags = ["front-matter", "author", "duplicate"]
draft = false
+++

The duplication can happen in real use if an Org setup file has the
same author as in the Org document where that file is included using
`SETUPFILE`. This test mimics this case:

```org
#+setupfile: some-file.org # some-file.org contains "#+author: Foo Bar"
#+author: Foo Bar
```
