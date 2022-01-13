+++
title = "Coderef"
description = """
  Support anchoring the source and example block lines using Org coderef
  links.
  """
tags = ["src-block", "coderef", "annotation", "example-block"]
draft = false
+++

See [info:org#Literal Examples](org#Literal%20Examples).

In literal examples, Org interprets strings like `(ref:name)` as
labels, and use them as targets for special hyperlinks like
`[[(name)]]` --- i.e., the reference name enclosed in single
parenthesis.

> Line numbers are always enabled if coderefs are used. The code blocks
> are exported as if the user always added the `-n` switches to the
> source or example block header.


## Source block {#source-block}


### Default line nums with coderef labels {#default-line-nums-with-coderef-labels}

{{< highlight emacs-lisp "linenos=table, anchorlinenos=true, lineanchors=org-coderef--c1cbed" >}}
(save-excursion                 (sc)
   (goto-char (point-min))      (jump)
{{< /highlight >}}

In line [sc](#org-coderef--c1cbed-1) we remember the current position. [Line jump](#org-coderef--c1cbed-2) jumps to
point-min.


### Default line nums without coderef labels {#default-line-nums-without-coderef-labels}

{{< highlight emacs-lisp "linenos=table, anchorlinenos=true, lineanchors=org-coderef--ea1413" >}}
(save-excursion
   (goto-char (point-min))
{{< /highlight >}}

In line [1](#org-coderef--ea1413-1) we remember the current position. [Line 2](#org-coderef--ea1413-2) jumps to
point-min.


### Custom line nums without coderef labels {#custom-line-nums-without-coderef-labels}

{{< highlight emacs-lisp "linenos=table, linenostart=20, anchorlinenos=true, lineanchors=org-coderef--cc4270" >}}
(save-excursion
   (goto-char (point-min))
{{< /highlight >}}

In line [20](#org-coderef--cc4270-20) we remember the current position. [Line 21](#org-coderef--cc4270-21) jumps to
point-min.


### Custom line nums without coderef labels and with highlighting {#custom-line-nums-without-coderef-labels-and-with-highlighting}

{{< highlight emacs-lisp "linenos=table, linenostart=20, hl_lines=2, anchorlinenos=true, lineanchors=org-coderef--a1ac71" >}}
(save-excursion
   (goto-char (point-min))
{{< /highlight >}}

[Line 21](#org-coderef--a1ac71-21) jumps to point-min.


### Reference to lines **before** code block {#reference-to-lines-before-code-block}

In line [1](#org-coderef--4489bc-1) we remember the current position. [Line 2](#org-coderef--4489bc-2) jumps to
point-min.

{{< highlight emacs-lisp "linenos=table, anchorlinenos=true, lineanchors=org-coderef--4489bc" >}}
(save-excursion
   (goto-char (point-min))
{{< /highlight >}}


## Example block {#example-block}

{{< highlight text "linenos=table, linenostart=20, anchorlinenos=true, lineanchors=org-coderef--942ea6" >}}
(save-excursion
   (goto-char (point-min))
{{< /highlight >}}

[Line 21](#org-coderef--942ea6-21) jumps to point-min.
