+++
title = "Org Babel Results"
tags = ["src-block", "babel", "results", "indentation", "example-block", "fixed-block"]
draft = false
+++

## Small Results block {#small-results-block}

Below also tests that the indentation in **results** blocks is
preserved.

```python
str = 'a\tbc'
print(str[1:])
```

```text
	bc
```

The whitespace before "bc" in the results block above should be
preserved.


## Big Results block {#big-results-block}

```nim
echo "abc\ndef\nghi\njkl\nmno\npqr\nstu\nvwx\nyz0\n123\n456\n789"
```

```text
abc
def
ghi
jkl
mno
pqr
stu
vwx
yz0
123
456
789
```
