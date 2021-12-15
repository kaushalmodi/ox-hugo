+++
title = "Hugo keyword"
description = "Stuff followed the `#+hugo:` exports as-is except when it is \"more\""
tags = ["body", "keyword", "hugo"]
draft = false
+++

As the content following `#+hugo:` exports as-is to the Markdown, that
content should be what Hugo can parse (i.e. Markdown, shortcodes,
etc).

This Org snippet:

```org
#+hugo: This `relref` links to [this same page]({​{< relref "hugo-keyword" >}}).
```

renders as below:

This `relref` links to [this same page]({{< relref "hugo-keyword" >}}).
