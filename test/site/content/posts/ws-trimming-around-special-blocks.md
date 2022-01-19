+++
title = "Whitespace trimming around special blocks"
description = "Whitespace trimming using special markers <>, < or > after special block names."
tags = ["special-block", "whitespace", "trimming"]
draft = false
+++

## No trimming {#no-trimming}

Below Org block:

```org
line 1
#+begin_mark
abc def
#+end_mark
line 2
```

exports and renders as:

line 1

<mark>abc def</mark>

line 2


## Whitespace trimmed before and after the `mark` special block using `<>` suffix {#whitespace-trimmed-before-and-after-the-mark-special-block-using-suffix}

Below Org block:

```org
line 1
#+begin_mark<>
abc def
#+end_mark<>
line 2
```

exports and renders as:

line 1 <mark>abc def</mark> line 2


## Whitespace trimmed only before the `mark` special block using `<` suffix {#whitespace-trimmed-only-before-the-mark-special-block-using-suffix}

Below Org block:

```org
line 1
#+begin_mark<
abc def
#+end_mark<
line 2
```

exports and renders as:

line 1 <mark>abc def</mark>

line 2


## Whitespace trimmed only after the `mark` special block using `>` suffix {#whitespace-trimmed-only-after-the-mark-special-block-using-suffix}

Below Org block:

```org
line 1
#+begin_mark>
abc def
#+end_mark>
line 2
```

exports and renders as:

line 1

<mark>abc def</mark> line 2


## Use `<span>` tag if trimming detected {#use-span-tag-if-trimming-detected}

Below Org block:

```org
line 1
#+begin_sidenote
abc def
#+end_sidenote
line 2
```

export with `<div>` tags by default:

```html { linenos=table, linenostart=1 }
line 1

<div class="sidenote">

abc def

</div>

line 2
```

and render as:

line 1

<div class="sidenote">

abc def

</div>

line 2

But if a trimming marker (any of them) is detected, it switches to
using the `<span>` tag. So the below Org block:

```org
line 1
#+begin_sidenote<>
abc def
#+end_sidenote<>
line 2
```

will export to:

```html { linenos=table, linenostart=1 }
line 1 <span class="sidenote">abc def</span> line 2
```

and render as:

line 1 <span class="sidenote">abc def</span> line 2
