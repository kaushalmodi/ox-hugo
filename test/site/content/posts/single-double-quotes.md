+++
title = "Single and Double quotes"
date = 2017-07-22
tags = ["body"]
draft = false
+++

The strings in these two columns should look the exact same.

|   | Rendered Actual                  | Rendered Expection               |
|---|----------------------------------|----------------------------------|
| 1 | 'This'                           | ‘This’                           |
| 2 | "This"                           | “This”                           |
| 3 | "It's"                           | “It’s”                           |
| 4 | 'It's'                           | ‘It’s’                           |
| 5 | "<http://ox-hugo.scripter.co/>"  | “<http://ox-hugo.scripter.co/>”  |
| 6 | "<http://ox-hugo.scripter.co/>". | “<http://ox-hugo.scripter.co/>”. |

**Note:** There is a rendering issue is Row 5 above. That seems to be a
 corner case, because notice that Row 6 looks fine just because there
 was a trailing period. _Will live with this issue for now._
