+++
title = "Links to source blocks"
description = "Test for internal links to source block references."
tags = ["links", "internal-links", "src-block"]
draft = false
+++

From [(org) Internal Links](https://orgmode.org/manual/Internal-Links.html),

>    If no dedicated target exists, the link will then try to match the
> exact name of an element within the buffer.  Naming is done with the
> ‘#+name’ keyword, which has to be put in the line before the element it
> refers to, as in the following example
>
> ```text
> #+name: My Target
> | a  | table      |
> |----+------------|
> | of | four cells |
> ```

Also, when targeting a `#+name` keyword, **`#+caption` keyword is
mandatory in order to get proper numbering** for <span class="underline">source blocks</span>,
<span class="underline">images</span> and <span class="underline">tables</span>.

So the below code block:

````org
#+caption: Hello
#+name: code__hello
#+begin_src emacs-lisp
(message "Hello")
#+end_src

*Here we refer to [[code__helloagain]].*

#+include: "../misc/common.org::#lorem-ipsum" :only-contents t

#+caption: Hello Again
#+name: code__helloagain
#+begin_src emacs-lisp
(message "Hello again")
#+end_src

*Here we refer to [[code__hello]].*
````

will output below (_lorem-ipsum_ added to increase page content so
that the link jump is evident):

<a id="code-snippet--hello"></a>
````emacs-lisp
(message "Hello")
````

<div class="src-block-caption">
  <span class="src-block-number"><a href="#code-snippet--hello">Code Snippet 1</a></span>:
  Hello
</div>

**Here we refer to [2](#code-snippet--helloagain).**

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque et
quam metus. Etiam in iaculis mi, sit amet pretium magna. Donec ut dui
mi. Maecenas pharetra sapien nunc, ut mollis enim aliquam quis. Nam at
ultricies metus. Nulla tempor augue in vestibulum tristique. Phasellus
volutpat pharetra metus quis suscipit. Morbi maximus sem dolor, id
accumsan ipsum commodo non.

Fusce quam ligula, gravida ac dui venenatis, bibendum commodo
lorem. Duis id elit turpis. Integer sed diam nibh. Donec tempus
lacinia odio, a laoreet velit dictum id. Suspendisse efficitur euismod
purus et porttitor. Aliquam sit amet tellus mauris. Mauris semper
dignissim nibh, faucibus vestibulum purus varius quis. Suspendisse
potenti. Cras at ligula sit amet nunc vehicula condimentum quis nec
est. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices
posuere cubilia Curae; Donec iaculis, neque sit amet maximus rhoncus,
nisl tortor convallis ante, ut mollis purus augue ut justo. Praesent
felis urna, volutpat sit amet posuere dictum, luctus quis nibh. Proin
et tristique ipsum, in aliquam ante.

Aenean eget ex mauris. Cras ut tempor quam. Curabitur eget nulla
laoreet, bibendum neque porta, tempus nulla. Ut tellus nisi, semper eu
ligula pretium, aliquam accumsan dolor. Nunc fermentum cursus arcu eu
suscipit. Nam dolor tellus, efficitur sed condimentum at, fringilla
eget nisi. Nulla luctus metus felis. Suspendisse potenti. Cras lacinia
orci nec dui sodales commodo. Donec tellus arcu, congue porta ultrices
non, pretium et sapien. Proin mattis risus dignissim feugiat
tristique. Donec nibh lorem, facilisis id posuere ut, varius ac
urna. Etiam ultrices dignissim mauris, quis venenatis ex semper ut.

Curabitur id fermentum erat, rhoncus scelerisque est. Sed pulvinar,
nulla sed sollicitudin scelerisque, ipsum erat sollicitudin dolor, ut
commodo arcu justo vel libero. Curabitur turpis dolor, fermentum ut
elit a, vehicula commodo nunc. Sed sit amet blandit nulla, quis
sodales massa. Donec lobortis, urna vel volutpat ullamcorper, mauris
est efficitur nulla, et suscipit velit dui at metus. Aliquam id sem
sed metus tristique scelerisque nec vitae odio. Phasellus a
pellentesque libero, vel convallis metus. Sed nec fringilla magna, non
varius ex. Sed interdum eleifend ligula, quis porta enim ultrices
a. Donec hendrerit diam ac elementum tincidunt.

Pellentesque eget nisl rhoncus, malesuada justo nec, suscipit
quam. Nam sodales mauris eu bibendum suscipit. Vivamus sodales dui
lorem, scelerisque pellentesque diam fermentum sed. Etiam fermentum
nisl id nisl blandit, sit amet semper erat ultricies. Nulla tincidunt
nulla metus, eu imperdiet lorem malesuada sagittis. Maecenas accumsan
risus sed ante eleifend, vitae pretium leo porta. Suspendisse vitae
eros vitae dui ornare condimentum id sit amet mauris. Etiam tincidunt
consequat risus, eu posuere mi. Donec ut nunc eu massa porttitor
suscipit nec nec neque. Suspendisse vitae tincidunt justo, sed
molestie velit. Nullam pellentesque convallis ante, vel posuere libero
blandit in.

<a id="code-snippet--helloagain"></a>
````emacs-lisp
(message "Hello again")
````

<div class="src-block-caption">
  <span class="src-block-number"><a href="#code-snippet--helloagain">Code Snippet 2</a></span>:
  Hello Again
</div>

**Here we refer to [1](#code-snippet--hello).**
