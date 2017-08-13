+++
title = "Spaces in tags"
tags = ["abc def", "abc def ghi", "abc def ghi jkl"]
draft = false
+++

The Org tags do not allow spaces. So the trick we use is replace
**double** underscores with spaces.

So an Org tag `abc__def` becomes Hugo tag `abc def`.
