+++
title = "Plain lists with ATTR_HTML"
tags = ["lists", "attr_html", "attr_css", "custom-counter"]
draft = false
+++

## Unordered lists {#unordered-lists}

<style>.red-text { color: red;  }</style>

<div class="red-text">

-   Red list item 1
-   Red list item 2

</div>
<!--listend-->

<style>.green-text { color: green;  }</style>

<div class="green-text">

-   Green list item 1
-   Green list item 2

</div>


## Ordered lists {#ordered-lists}

<div class="green-text">

1.  Green ordered list item 1
2.  Green ordered list item 2

</div>

_The `green-text` style is defined in the list above this one._


### Ordered list with custom counter {#ordered-list-with-custom-counter}

<style>.blue-text { color: blue;  }</style>

<ol class="org-ol blue-text">
<li>Blue list item 1</li>
<li>Blue list item 2</li>
<li value="10">Blue list item 10</li>
</ol>


## Definition/descriptive lists {#definition-descriptive-lists}

<div class="red-text">

Defn A
: Something A in red

Defn B
: Something B in red

</div>

_The `red-text` style is defined in the first list above._
