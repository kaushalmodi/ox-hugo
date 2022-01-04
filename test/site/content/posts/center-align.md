+++
title = "Center align"
tags = ["body", "force", "line-break", "center-align"]
draft = false
+++

From [`C-h i g (org) Paragraphs`](https://orgmode.org/manual/Paragraphs.html):

> If you would like to center some text, do it like this:
>
> ```org
> #+begin_center
> Everything should be made as simple as possible, \\
> but not any simpler
> #+end_center
> ```

<style>.org-center { margin-left: auto; margin-right: auto; text-align: center; }</style>

<div class="org-center">

Everything should be made as simple as possible, <br />
but not any simpler

</div>


## Testing emphasis in Org center block {#testing-emphasis-in-org-center-block}

<style>.org-center { margin-left: auto; margin-right: auto; text-align: center; }</style>

<div class="org-center">

**bold** <br />
_italics_ <br />
~~strikethrough~~

</div>
