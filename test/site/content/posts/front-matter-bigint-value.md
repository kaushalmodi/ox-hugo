+++
title = "Front-matter values with BigInts"
description = """
  Test that front-matter values that are integers represented as
  strings, but cannot be stored as 64-bit signed integers are
  left as strings.
  """
tags = ["custom-fm", "bigint", "toml"]
draft = false
small_int = 234
big_int = "10040216507682529280"
+++

In this test, the small integer "234" (i.e. the one that can be
represented as a 64-bit signed integer) is saved as an integer in the
TOML front-matter.

But the big integer "10040216507682529280" which would need more than
64-bits to be stored as a signed integer is left as a string in the
TOML front-matter.
