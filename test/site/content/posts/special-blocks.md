+++
title = "Special Blocks"
tags = ["special-block"]
draft = false
+++

## HTML5 Element Blocks {#html5-element-blocks}


### Block without NAME, class or id {#block-without-name-class-or-id}

<article>
  <article></article>

This is _an article_.

</article>


### Block with NAME {#block-with-name}

<aside id="Aside-A">
  <aside></aside>

_Some_ **text** --- 1

| a | b | c |
|---|---|---|
| d | e | f |

</aside>


### Block with class and id {#block-with-class-and-id}

<section class="my-section" id="section-a">
  <section></section>

_Some_ **text** --- 2

| g | h | i |
|---|---|---|
| j | k | l |

</section>


### An inline HTML5 element {#an-inline-html5-element}

Unmarked.

<mark>
_Some_ **marked** text --- 2.5.
</mark>

Unmarked again.


## DIV-wrapped Blocks {#div-wrapped-blocks}


### DIV without NAME, class or id {#div-without-name-class-or-id}

<div class="something">
  <div></div>

This is _some text_ wrapped in a `div` block with class `something`.

</div>


### DIV with NAME {#div-with-name}

<div class="foo" id="Foo-A">
  <div></div>

_Some_ **text** --- 3

| m | n | o |
|---|---|---|
| p | q | r |

</div>


### DIV with class and id {#div-with-class-and-id}

<div class="my-bar bar" id="bar-a">
  <div></div>

_Some_ **text** --- 4

| s | t | u |
|---|---|---|
| v | w | x |

</div>
