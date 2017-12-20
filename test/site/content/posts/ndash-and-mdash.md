+++
title = "ndash `and` mdash"
date = 2017-07-22
tags = ["body"]
draft = false
+++

The strings in these two columns should look the exact same.

|   | Character | Rendered Actual | Rendered Expection |
|---|-----------|-----------------|--------------------|
| 1 | Hyphen    | a - b           | a - b              |
| 2 | Ndash     | a -- b          | a – b              |
| 3 | Mdash     | a --- b         | a — b              |
| 4 | Ellipsis  | a ... b         | a … b              |


## Title sanitization {#title-sanitization}

This post has italics, monospace and bold in the title. This is to
test that those markup characters **do not** end up in the `title` front
matter of the post because HTML does not allow markup in the `<title>`
section.

So the title of this post should read as "ndash and mdash".
