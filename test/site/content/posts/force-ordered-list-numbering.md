+++
title = "Force ordered list numbering"
date = 2017-08-01
tags = ["lists", "custom-counter"]
draft = false
+++

Ordered lists with custom counter.

1.  This will be 1.
2.  This will be 2.

<!--listend-->

10. This will be 10!
11. This will be 11.

<!--listend-->

<ol class="org-ol">
<li value="17">This will be 17!</li>
<li>This will be 18.</li>
<li value="123">This will be 123!</li>
<li>This will be 124.</li>
</ol>

1.  This will be 1 again.
2.  This will be 2.

Another example:

<ol class="org-ol">
<li>This will be 1.</li>
<li value="3">This will be 3!</li>
<li value="7">This will be 7!</li>
<li value="100">This will be 100!</li>
</ol>

Ordered list numbers larger then 99:

100. This will be 100!
101. This will be 101.
102. This will be 102.

<!--listend-->

999999997. This will be 999999997!
999999998. This will be 999999998.
999999999. This will be 999999999. CommonMark 0.30 [allows](https://spec.commonmark.org/0.30/#ordered-list-marker) an ordered
    list marker to be at max 9 digits long.

See [(org) Plain Lists](https://orgmode.org/manual/Plain-Lists.html) to read more about plain lists in Org.
