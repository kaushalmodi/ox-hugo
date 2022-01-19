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
