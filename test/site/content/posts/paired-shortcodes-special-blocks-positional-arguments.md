+++
title = "Paired shortcodes using special blocks (Positional Arguments)"
description = "Tests for paired shortcodes with positional arguments."
tags = ["shortcode", "paired", "special-block", "positional", "arguments"]
draft = false
+++

Test case for feature requested in `ox-hugo` Issue #[119](https://github.com/kaushalmodi/ox-hugo/issues/119).


## Paired markdown shortcode {#paired-markdown-shortcode}

The `alert` shortcode takes 1 positional argument.

<style>
 .alert {
     padding: 15px;
     margin-bottom: 20px;
     border: 1px solid transparent;
     border-radius: 4px;
 }
 div.alert {
     border-radius: 10px;
     margin-bottom: 1rem;
 }

 div.alert p {
     position: relative;
     display: block;
     font-size: 1rem;
     margin-left: 2rem;
     margin-top: 0;
     margin-bottom: 0;
 }

 div.alert a {
     color: rgba(255,255,255,0.9);
     text-decoration: none;
     border-bottom: solid 1px #e4e4e4;
     transition: color 0.2s ease-in-out, border-color 0.2s ease-in-out;
 }

 div.alert a:hover {
     border-bottom-color: transparent;
     color: rgba(255,255,255,0.5) !important;
 }

 .alert-note {
     color: #fff;
     background-color: #03A9F4; /* Material LightBlue500 */
     border-color: #bce8f1;
 }

 .alert-warning {
     color: #fff;
     background-color: #f44336; /* Material Red500 */
     border-color: #ebccd1;
 }
</style>

<style>
 div.alert p:first-child::before {
     position: absolute;
     top: -0.5rem;
     left: -2rem;
     font-family: 'FontAwesome';
     font-size: 1.5rem;
     color: #fff;
     /* content: '\f05a'; */
     content: '🛈';
     width: 1.5rem;
     text-align: center;
 }

 div.alert-warning p:first-child:before {
     /* content: '\f071'; */
     content: '⚠';
 }
</style>

{{% alert note %}}
Content **with** _emphasis_ characters is rendered.
{{% /alert %}}

---

Below is the about same as above, except that `warning` attribute
(argument to the exported shortcode) is used instead of `note`.

{{% alert warning %}}
Content **with** _emphasis_ characters is rendered.
{{% /alert %}}


## Paired non-markdown (default) shortcode {#paired-non-markdown--default--shortcode}

The `myshortcode-pos` takes 2 positional arguments. In the below
example, the double-quoted `"foo bar"` will be the _first_ argument,
and `"zoo"` will be the _second_.

{{< myshortcode-pos "foo bar" zoo >}}
The Markdown _emphasis_ characters are !! NOT !! rendered.
{{< /myshortcode-pos >}}

`ox-hugo` Issue #[377](https://github.com/kaushalmodi/ox-hugo/issues/377)

{{< katex display >}}
E = -J \sum\_{i=1}^N s\_i s\_{i+1}
{{< /katex >}}
