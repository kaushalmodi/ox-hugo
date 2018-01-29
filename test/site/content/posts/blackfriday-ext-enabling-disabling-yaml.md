---
title: "Enabling/Disabling extensions (YAML)"
date: 2017-08-02
tags: ["blackfriday", "yaml"]
draft: false
blackfriday:
  extensionsmask: ["fencedCode", "strikethrough"]
  extensions: ["tabSizeEight", "hardLineBreak"]
  plainIDAnchors: false
  hrefTargetBlank: true
  smartDashes: false
  fractions: false
  angledQuotes: true
---

Extensions enabled
: `tabSizeEight`, `hardLineBreak`

Extensions disabled
: `fencedCode`, `strikethrough`


## Angled quotes enabled {#angled-quotes-enabled}

"this"


## Hard line break enabled {#hard-line-break-enabled}

a
b
c


## Plain ID Anchors disabled {#plain-id-anchors-disabled}

Check the ID for all the headings in this post's HTML. The ID's will
look something like:

```html
<h2 id="plain-id-anchors-disabled:c94b2acd735ed6a466ef85be48bdea8c">Plain ID Anchors disabled</h2>
```

where `:c94b2acd735ed6a466ef85be48bdea8c` is the document ID.


## Fractions disabled {#fractions-disabled}

2/5


## Smart dashes disabled {#smart-dashes-disabled}

a--b	c--d


## Fenced code disabled {#fenced-code-disabled}

Below, the code block language name will show up before the code.

```emacs-lisp
(message "Hello")
```


## Strikethrough disabled {#strikethrough-disabled}

~~not-canceled~~
