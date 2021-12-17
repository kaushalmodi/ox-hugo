+++
title = "Links to tables"
description = "Test for internal links to table references."
tags = ["links", "internal-links", "table", "caption"]
draft = false
+++

**Note**: When targeting a `#+name` keyword, **`#+caption` keyword is
mandatory in order to get proper numbering** for <span class="underline">source blocks</span>,
<span class="underline">images</span> and <span class="underline">tables</span>.

```org
#+caption: Simple table 1
#+name: tab__simple1
| a | b |
| c | d |

*Here we refer to [[tab__simple2]].*

#+include: "../misc/common.org::#lorem-ipsum" :only-contents t

Here's another table:

#+caption: Simple table 2
#+name: tab__simple2
| e | f |
| g | h |

*Here we refer to [[tab__simple1]].*
```

will output below (_lorem-ipsum_ added to increase page content so
that the link jump is evident):

<a id="table--simple1"></a>
<div class="table-caption">
  <span class="table-number"><a href="#table--simple1">Table 1</a></span>:
  Simple table 1
</div>

| a | b |
|---|---|
| c | d |

**Here we refer to [2](#table--simple2).**

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

Here's another table:

<a id="table--simple2"></a>
<div class="table-caption">
  <span class="table-number"><a href="#table--simple2">Table 2</a></span>:
  Simple table 2
</div>

| e | f |
|---|---|
| g | h |

**Here we refer to [1](#table--simple1).**

---

Reference: [(org) Internal Links](https://orgmode.org/manual/Internal-Links.html).
