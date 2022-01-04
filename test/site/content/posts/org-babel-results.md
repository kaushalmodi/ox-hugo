+++
title = "Org Babel Results"
description = """
  Testing the export of Org Babel `#+results:` blocks with and without
  wrapping with HTML attributes.
  """
tags = ["src-block", "babel", "results", "indentation", "example-block", "fixed-block"]
draft = false
+++

## "Fixed block" Results block {#fixed-block-results-block}

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


### "Fixed block" Results block with `#+attr_html` {#fixed-block-results-block-with-plus-attr-html}

```python
str = 'd\tef'
print(str[1:])
```

<style>.results-fixed-block { color: blue;  }</style>

<div class="results-fixed-block">

```text
	ef
```
</div>

Above results block will be in <span class="underline">blue</span> text.


## "Example block" Results block {#example-block-results-block}

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


### "Example block" Results block with `#+attr_html` {#example-block-results-block-with-plus-attr-html}

```nim
echo "ABC\nDEF\nGHI\nJKL\nMNO\nPQR\nSTU\nVWX\nYZ0\n123\n456\n789"
```

<style>.results-example-block { color: green;  }</style>

<div class="results-example-block">

```text
ABC
DEF
GHI
JKL
MNO
PQR
STU
VWX
YZ0
123
456
789
```
</div>

Above results block will be in <span class="underline">green</span> text.
