+++
title = "Details and summary"
description = "Details disclosure elements [`<details>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details) and `<summary>`."
tags = ["special-block", "details", "summary", "disclosure"]
draft = false
+++

<style>details summary { color: green; }</style>

<style>details .details { color: blue; }</style>

These can be conveniently created using the Org Special Block
`#+begin_details` .. `#+end_details`.

The summaries in these "details" Org Special blocks should be wrapped
in a "summary" Org Special block `#+begin_summary` .. `#+end_summary`.


## Closed details disclosure (default) {#closed-details-disclosure--default}


### Summary + Details {#summary-plus-details}

<details>
<summary>Here is the <b>summary</b>.

Summary can also contain markup (like <b>bold</b>, <i>italics</i>, etc.).</summary>
<div class="details">

Here are some _details_.

-   list element 1
-   list element 2

Here's a paragraph.

And another paragraph.

```emacs-lisp
(message "a code block")
```
</div>
</details>


### No summary, only details {#no-summary-only-details}

In the situation where the summary divider is absent i.e. when user
hasn't provided a summary, the browser will use a default summary
string (usually "Details").

<details>
<div class="details">

Here are some _details_.

-   list element 1
-   list element 2

Here's a paragraph.

And another paragraph.

```emacs-lisp
(message "a code block")
```
</div>
</details>


### Only summary, no details {#only-summary-no-details}

In this case where only the "summary" exists, the browser will still
render the collapsing triangle. But nothing will show up when you
uncollapse it.. _as there are no details_.

<details>
<summary>Here is the <b>summary</b>.

Summary can also contain markup (like <b>bold</b>, <i>italics</i>, etc.).</summary>
<div class="details">
</div>
</details>


### Details containing only code block {#details-containing-only-code-block}

<details>
<summary>This is Summary.</summary>
<div class="details">

```emacs-lisp
(message "This is in details")
```
</div>
</details>


## Open by default disclosure widget {#open-by-default-disclosure-widget}

The details disclosures are closed by default, add `#+attr_html: :open
t` right below the details special block to have the disclosures open
by default.


### Summary + Details (Open) {#summary-plus-details--open}

<details open>
<summary>Here is the <b>summary</b>.

Summary can also contain markup (like <b>bold</b>, <i>italics</i>, etc.).</summary>
<div class="details">

Here are some _details_.

-   list element 1
-   list element 2

Here's a paragraph.

And another paragraph.

```emacs-lisp
(message "a code block")
```
</div>
</details>


### No summary, only details (Open) {#no-summary-only-details--open}

<details open>
<div class="details">

Here are some _details_.

-   list element 1
-   list element 2

Here's a paragraph.

And another paragraph.

```emacs-lisp
(message "a code block")
```
</div>
</details>


### Only summary, no details (Open) {#only-summary-no-details--open}

<details open>
<summary>Here is the <b>summary</b>.

Summary can also contain markup (like <b>bold</b>, <i>italics</i>, etc.).</summary>
<div class="details">
</div>
</details>


## Other attributes along with `open` attribute set to `t` {#other-attributes-along-with-open-attribute-set-to-t}

Test that other attributes, if present along with `:open t`, are also retained.

<details open class="foo">
<summary>This is Summary.</summary>
<div class="details">

Here are the _details_.
</div>
</details>


## Value of `open` attribute other than `t` {#value-of-open-attribute-other-than-t}

If the `open` attribute is set to any other value than `t`, it won't
be inserted in the `details` element.

<details class="foo">
<summary>This is Summary.</summary>
<div class="details">

Here are the _details_.
</div>
</details>
