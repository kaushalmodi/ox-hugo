+++
title = "Hard line break wrong case (TOML)"
date = 2017-08-02
tags = ["blackfriday", "toml"]
draft = false
[blackfriday]
  extensions = ["hardLineBreak"]
+++

The Blackfriday `hardLineBreak` extension is enabled here even where
user used the wrong case in the extension name:

```text
:EXPORT_HUGO_BLACKFRIDAY: :extensions hardlinebreak
```

instead of:

```text
:EXPORT_HUGO_BLACKFRIDAY: :extensions hardLineBreak
```

The Blackfriday extension names are case-sensitive. So even though,
the wrong case is used in the Org property drawer, `ox-hugo` ensures
that the Markdown front matter is written in the correct case!
:raised_hands:.

a
b
c

Above, _a_, _b_ and _c_ must appear on separate lines.
