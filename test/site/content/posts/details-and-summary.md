+++
title = "Details and summary"
description = "Details disclosure elements [`<details>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details) and `<summary>`."
tags = ["special-block", "details", "summary", "disclosure"]
draft = false
+++

These can be conveniently created using the Org Special Block
`#+begin_details` .. `#+end_details`.

The summaries in these "details" Org Special blocks should be wrapped
in a "summary" Org Special block `#+begin_summary` .. `#+end_summary`.


## Closed details disclosure (default) {#closed-details-disclosure--default}


### Summary + Details {#summary-details}

<details>
<summary>
Here is the **summary**.

While normally summaries wouldn't be across multiple paragraphs, we
are testing it that way as HTML supports that.

```emacs-lisp
(message
 (concat "This is part of the summary too.. "
         "just click anywhere in the summary to expand it."))
```
</summary>
<p class="details">

Here are some _details_.

-   list element 1
-   list element 2

Here's a paragraph.

And another paragraph.

```emacs-lisp
(message "a code block")
```
</p>
</details>


### No summary, only details {#no-summary-only-details}

In the situation where the summary divider is absent i.e. when user
hasn't provided a summary, the browser will use a default summary
string (usually "Details").

<details>
<p class="details">Here are some _details_.

-   list element 1
-   list element 2

Here's a paragraph.

And another paragraph.

```emacs-lisp
(message "a code block")
```
</p>
</details>


### Only summary, no details {#only-summary-no-details}

In this case where only the "summary" exists, the browser will still
render the collapsing triangle. But nothing will show up when you
uncollapse it.. _as there are no details_.

<details>
<summary>
Here is the **summary**.

While normally summaries wouldn't be across multiple paragraphs, we
are testing it that way as HTML supports that.

```emacs-lisp
(message
 (concat "This is part of the summary too.. "
         "just click anywhere in the summary to expand it."))
```
</summary>
<p class="details">
</p>
</details>


### Details containing only code block {#details-containing-only-code-block}

<details>
<summary>
This is **Summary**.
</summary>
<p class="details">

```emacs-lisp
(message "This is in details")
```
</p>
</details>


### Summary containing only code block {#summary-containing-only-code-block}

<details>
<summary>
```emacs-lisp
(message "This is in summary")
```
</summary>
<p class="details">

Here are the _details_. This is obviously a pathological test case.
</p>
</details>


## Open by default disclosure widget {#open-by-default-disclosure-widget}

The details disclosures are closed by default, add `#+attr_html: :open
t` right below the details special block to have the disclosures open
by default.


### Summary + Details (Open) {#summary-details--open}

<details open>
<summary>
Here is the **summary**.

While normally summaries wouldn't be across multiple paragraphs, we
are testing it that way as HTML supports that.

```emacs-lisp
(message
 (concat "This is part of the summary too.. "
         "just click anywhere in the summary to expand it."))
```
</summary>
<p class="details">

Here are some _details_.

-   list element 1
-   list element 2

Here's a paragraph.

And another paragraph.

```emacs-lisp
(message "a code block")
```
</p>
</details>


### No summary, only details (Open) {#no-summary-only-details--open}

<details open>
<p class="details">Here are some _details_.

-   list element 1
-   list element 2

Here's a paragraph.

And another paragraph.

```emacs-lisp
(message "a code block")
```
</p>
</details>


### Only summary, no details (Open) {#only-summary-no-details--open}

<details open>
<summary>
Here is the **summary**.

While normally summaries wouldn't be across multiple paragraphs, we
are testing it that way as HTML supports that.

```emacs-lisp
(message
 (concat "This is part of the summary too.. "
         "just click anywhere in the summary to expand it."))
```
</summary>
<p class="details">
</p>
</details>


## Other attributes along with `open` attribute set to `t` {#other-attributes-along-with-open-attribute-set-to-t}

Test that other attributes, if present along with `:open t`, are also retained.

<details open class="foo">
<summary>
This is **summary**.
</summary>
<p class="details">

Here are the _details_.
</p>
</details>


## Value of `open` attribute other than `t` {#value-of-open-attribute-other-than-t}

If the `open` attribute is set to any other value than `t`, it won't
be inserted in the `details` element.

<details class="foo">
<summary>
This is **summary**.
</summary>
<p class="details">

Here are the _details_.
</p>
</details>
