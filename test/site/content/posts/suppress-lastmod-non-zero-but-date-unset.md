+++
title = "Suppress lastmod non-zero, but date unset"
description = """
  Ensure that nil value of parsed DATE is also handled in the "suppress
  lastmod" logic.
  """
tags = ["autoset", "suppress", "lastmod", "nonzero", "date", "unset"]
draft = false
+++

Test for the bug-fix when `date-str` (internal variable) could be
_nil_.

[Ref](https://github.com/kaushalmodi/ox-hugo/pull/197#issuecomment-421533876)
