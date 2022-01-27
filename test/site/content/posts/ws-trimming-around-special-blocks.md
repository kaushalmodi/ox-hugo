+++
title = "Whitespace trimming around special blocks"
description = "Whitespace trimming using ~#+header: :trim-pre t :trim-post t~ before special blocks."
tags = ["special-block", "whitespace", "trimming"]
draft = false
+++

By default (in `org-hugo-special-block-type-properties`), the "mark"
special block type has `:trim-pre` and `:trim-post` both set to `t`,
because typically the `<mark>` is used to highlight words and phrases
mid-sentence and we wouldn't want to introduce a paragraph break
before or after a `<mark>` element.


## Whitespace trimmed before and after the `mark` special block (default) {#whitespace-trimmed-before-and-after-the-mark-special-block--default}

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
<mark>abc def</mark> line 2


## Whitespace trimmed only before the `mark` special block {#whitespace-trimmed-only-before-the-mark-special-block}

Below Org block:

```org
line 1
#+header: :trim-post nil
#+begin_mark
abc def
#+end_mark
line 2
```

exports and renders as:

line 1
<mark>abc def</mark>

line 2


## Whitespace trimmed only after the `mark` special block {#whitespace-trimmed-only-after-the-mark-special-block}

Below Org block:

```org
line 1
#+header: :trim-pre nil
#+begin_mark
abc def
#+end_mark
line 2
```

exports and renders as:

line 1

<mark>abc def</mark> line 2


## No trimming {#no-trimming}

Below Org block:

```org
line 1
#+header: :trim-pre nil :trim-post nil
#+begin_mark
abc def
#+end_mark
line 2
```

exports and renders as:

line 1

<mark>abc def</mark>

line 2


## Use `<span>` tag if trimming detected {#use-span-tag-if-trimming-detected}

Below Org block:

```org
line 1
#+begin_foo
abc def
#+end_foo
line 2
```

exports with `<div>` tags by default:

```html { linenos=table, linenostart=1 }
line 1

<div class="foo">

abc def

</div>

line 2
```

and renders as:

line 1

<div class="foo">

abc def

</div>

line 2

But if any of the trimming options are set in the header, it switches
to using the `<span>` tag. So the below Org block:

```org
line 1
#+header: :trim-pre t :trim-post t
#+begin_foo
abc def
#+end_foo
line 2
```

will export to:

```html { linenos=table, linenostart=1 }
line 1 <span class="foo">abc def</span> line 2
```

and render as:

line 1
<span class="foo">abc def</span> line 2
