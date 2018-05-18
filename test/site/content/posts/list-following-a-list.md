+++
title = "List following a list"
date = 2017-07-31
tags = ["lists"]
draft = false
+++

You need to force end of list when you have something like an
unordered list immediately following an ordered list.

_The easiest and cleanest way to do that is adding a comment between
those lists._ -- [Reference](https://stackoverflow.com/a/8964494/1219634)

That would be the implementing in the Org exporter backend. But in
Org, two consecutive blank lines after a list ends the list.

In the below example, the _foo\*_ items would be in a different `<ul>`
element than the _bar\*_ items.


## Unordered list following an unordered list {#unordered-list-following-an-unordered-list}

-   foo1
-   foo2

<!--listend-->

-   bar1
-   bar2


## Unordered list following an ordered list {#unordered-list-following-an-ordered-list}

1.  foo3
2.  foo4

<!--listend-->

-   bar3
-   bar4


## Ordered list following an unordered list {#ordered-list-following-an-unordered-list}

-   foo5
-   foo6

<!--listend-->

1.  bar5
2.  bar6


## Ordered list following an ordered list {#ordered-list-following-an-ordered-list}

1.  foo1
2.  foo2

<!--listend-->

1.  bar1
2.  bar2


## Description list following an ordered list {#description-list-following-an-ordered-list}

-   foo1
-   foo2

<!--listend-->

bar1
: description

bar2
: description
