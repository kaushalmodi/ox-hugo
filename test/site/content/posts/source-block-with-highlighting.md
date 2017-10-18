+++
title = "Source blocks with highlighting"
tags = ["src-block"]
draft = false
+++

## Without line numbers {#source-blocks-with-highlighting-no-linenums}

{{< highlight emacs-lisp "hl_lines=1 3-5">}}
(message "This is line 1")
(message "This is line 2")
(message "This is line 3")
(message "This is line 4")
(message "This is line 5")
(message "This is line 6")
{{< /highlight >}}

Above highlighting looks weird as the highlighting spans the full
page/container width. This is probably a bug in Hugo.

The workaround is below.. **use line numbers too!**.


## With line numbers {#source-blocks-with-highlighting-with-linenums}

The highlighting looks better with line numbers enabled because then
the highlight is limited to the width of the HTML table rows (because
`ox-hugo` sets the `linenos=table` option in the `highlight` shortcode
by default when line numbers are enabled).

{{< highlight emacs-lisp "linenos=table, linenostart=1, hl_lines=1 3-5">}}
(message "This is line 1")
(message "This is line 2")
(message "This is line 3")
(message "This is line 4")
(message "This is line 5")
(message "This is line 6")
{{< /highlight >}}
