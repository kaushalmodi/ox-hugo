+++
title = "Whitespace trimming around special blocks"
tags = ["special-block", "whitespace", "trimming"]
draft = false
+++

## No trimming {#no-trimming}

line 1

<mark>abc def</mark>

line 2


## Whitespace trimmed before and after the `mark` special block using `<>` suffix {#whitespace-trimmed-before-and-after-the-mark-special-block-using-suffix}

line 1 <mark>abc def</mark> line 2


## Whitespace trimmed only before the `mark` special block using `<` suffix {#whitespace-trimmed-only-before-the-mark-special-block-using-suffix}

line 1 <mark>abc def</mark>

line 2


## Whitespace trimmed only after the `mark` special block using `>` suffix {#whitespace-trimmed-only-after-the-mark-special-block-using-suffix}

line 1

<mark>abc def</mark> line 2
