+++
title = "Details and summary"
description = "Details disclosure elements [`<details>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details) and `<summary>`."
tags = ["special-block", "details", "summary", "disclosure"]
draft = false
+++

These can be conveniently created using the Org Special Block
`#+begin_details` .. `#+end_details`.

To separate the summary from the details, use the summary-divider
string `---` at the **beginning of a new line**.


## Closed details disclosure (default) {#closed-details-disclosure--default}


### Summary + Details {#summary-details}

<details><summary>Some **Summary**
</summary><p class="details">
Here are some _details_.

-   list element 1
-   list element 2

Here's a paragraph.

And another paragraph.

```emacs-lisp
(message "a code block")
```
</p></details>


### No summary, only details {#no-summary-only-details}

In the situation where the summary divider is absent i.e. when user
hasn't provided a summary, the browser will use a default summary
string (usually "Details").

<details><p class="details">Here are some _details_.

-   list element 1
-   list element 2

Here's a paragraph.

And another paragraph.

```emacs-lisp
(message "a code block")
```
</p></details>


### Only summary, no details {#only-summary-no-details}

If the summary divider is the last string in this special block, it
will mean that this `<details>` element has only the _summary_ and no
_details_. In that case too, the browser will still render the
collapsing triangle. But nothing will show up when you uncollapse
it.. _as there no details_.

<details><summary>Some **Summary**
</summary><p class="details">
</p></details>


## Open by default disclosure widget {#open-by-default-disclosure-widget}

The details disclosures are closed by default, add `#+attr_html: open`
right below the details special block to have the disclosures open by
default.


### Summary + Details (Open) {#summary-details--open}

<details open><summary>Some **Summary**
</summary><p class="details">
Here are some _details_.

-   list element 1
-   list element 2

Here's a paragraph.

And another paragraph.

```emacs-lisp
(message "a code block")
```
</p></details>


### No summary, only details (Open) {#no-summary-only-details--open}

<details open><p class="details">Here are some _details_.

-   list element 1
-   list element 2

Here's a paragraph.

And another paragraph.

```emacs-lisp
(message "a code block")
```
</p></details>


### Only summary, no details (Open) {#only-summary-no-details--open}

<details open><summary>Some **Summary**
</summary><p class="details">
</p></details>
