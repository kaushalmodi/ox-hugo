+++
title = "Custom front matter in multiple lines"
date = 2017-07-24
tags = ["custom-fm"]
draft = false
foo = "bar"
baz = "zoo"
alpha = 1
beta = "two words"
gamma = 10
+++

From [**(org) Property Syntax**](https://orgmode.org/manual/Property-Syntax.html):

>    It is also possible to add to the values of inherited properties.
> The following results in the 'genres' property having the value
> "Classic Baroque" under the 'Goldberg Variations' subtree.

```text
* CD collection
** Classic
:PROPERTIES:
:GENRES: Classic
:END:
*** Goldberg Variations
:PROPERTIES:
:Title:     Goldberg Variations
:Composer:  J.S. Bach
:Artist:    Glen Gould
:Publisher: Deutsche Grammophon
:NDisks:    1
:GENRES+:   Baroque
:END:
```
