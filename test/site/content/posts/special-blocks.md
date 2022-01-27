+++
title = "Special Blocks"
tags = ["special-block"]
draft = false
+++

## HTML5 Elements {#html5-elements}


### Block without NAME, class or id {#block-without-name-class-or-id}

<article>

This is _an article_.

</article>


### Block with NAME {#block-with-name}

<aside id="Aside-A">

_Some_ **text** --- 1

| a | b | c |
|---|---|---|
| d | e | f |

</aside>


### Block with class and id {#block-with-class-and-id}

<section class="my-section" id="section-a">

_Some_ **text** --- 2

| g | h | i |
|---|---|---|
| j | k | l |

</section>


### Inline HTML5 elements {#inline-html5-elements}

Unmarked.
<mark>_Some_ **marked** text --- 2.5.</mark> Unmarked again.

<mark>Page Bundles of `page` [_Kind_](https://gohugo.io/templates/section-templates/#page-kinds) are always _leaf bundles_.. and vice versa.</mark>

Here's an inline HTML element `<cite>` inside a Quote block:

> He puts his claw against the divider. "Fist my bump."
>
> <cite>Andy Weir, Project Hail Mary</cite>

<progress max="100" value="70">70%</progress>


## DIV-wrapped Blocks {#div-wrapped-blocks}


### DIV without NAME, class or id {#div-without-name-class-or-id}

<div class="something">

This is _some text_ wrapped in a `div` block with class `something`.

</div>


### DIV with NAME {#div-with-name}

<div class="foo" id="Foo-A">

_Some_ **text** --- 3

| m | n | o |
|---|---|---|
| p | q | r |

</div>


### DIV with class and id {#div-with-class-and-id}

<div class="my-bar bar" id="bar-a">

_Some_ **text** --- 4

| s | t | u |
|---|---|---|
| v | w | x |

</div>
